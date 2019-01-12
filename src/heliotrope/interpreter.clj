(ns heliotrope.interpreter
  (:refer-clojure :exclude [eval reify load-file])
  (:import (java.io Writer)))


(def globals (atom {}))

(def code-cache (atom {}))

(defn register [name global]
  (swap! globals assoc name global))

(defprotocol IInvokable
  (invoke [f env args]))

(defprotocol IEffect
  (invoke-effect [f env args]))

(deftype DefaultHandler []
  IInvokable
  (invoke [_ env [f & args]]
    (invoke-effect f env args)))

(def ^:dynamic handler (->DefaultHandler))


(defprotocol Fexpr)

(defn fexpr? [f]
  (satisfies? Fexpr f))

(defrecord TailCall [f env args])

(defrecord Code [code]
  Object
  (toString [this]
    (str "Code<" (str code) ">")))

(defn code? [v]
  (instance? Code v))

(defn lift [v]
  (if (code? v)
    v
    (->Code v)))

(defn reify [v]
  (if (code? v)
    (:code v)
    v))

(defn quotev [v]
  (if (or (seq? v)
          (symbol? v))
    (list 'quote (reify v))
    v))

(declare eval)

(defn tail-call? [tc]
  (instance? TailCall tc))

(defn eval-notail
  ([form]
   (eval-notail {} form))
  ([env form]
   (loop [result (eval env form)]
     (if (tail-call? result)
       (recur (invoke (:f result) (:env result) (:args result)))
       result))))

(defn strip-namespace [sym]
  (if (namespace sym)
    (symbol (name sym))
    sym))

(defn lookup-symbol [env sym]
  (let [sym (strip-namespace sym)]
    (if (code? env)
      (lift `(~(reify env) ~sym))
      (if-let [[_ v] (find env sym)]
        v
        (if-let [[_ v] (find @globals sym)]
          v
          (throw (ex-info (str "Could not resolve symbol " sym)
                          {:symbol sym
                           :env    env})))))))

(defn eval-seq [env [f & args]]
  (let [fn (eval-notail env f)]
    (if (code? fn)
      (lift (list* (reify fn) (map reify args))))
    (if (fexpr? fn)
      (->TailCall fn env args)
      (->TailCall fn env (mapv #(eval-notail env %) args)))))

(defn eval
  [env form]
  (cond
    (symbol? form) (lookup-symbol env form)
    (seq? form) (eval-seq env form)
    :else form)
  )


(deftype Def []
  Fexpr
  IInvokable
  (invoke [f env [sym val]]
    (let [result (eval-notail env val)]
      (if (code? result)
        (lift `(~'def ~sym ~(reify result)))
        (swap! globals assoc sym result)))))

(register 'def (->Def))

(deftype Do []
  Fexpr
  IInvokable
  (invoke [f env body]
    (loop [[h & t] body
           codes []]
      (if t
        (let [result (eval-notail env h)]
          (if (code? result)
            (recur t (conj codes (reify result)))
            (recur t codes)))
        (if (seq codes)
          (lift `(~'do ~@codes ~(reify (eval-notail env h))))
          (eval env h))))))

(register 'do (->Do))

(deftype Lift []
  IInvokable
  (invoke [f env [val]]
    (lift (eval-notail env val))))

(register 'lift (->Lift))

(deftype If []
  Fexpr
  IInvokable
  (invoke [f env [test then else]]
    (let [result (eval-notail env test)]
      (if (code? result)
        (lift `(~'if ~(reify result)
                 ~(reify (eval-notail env then))
                 ~(reify (eval-notail env else))))
        (if result
          (eval env then)
          (eval env else))))))

(register 'if (->If))

(defmacro register-reorder-safe-fn
  "Wraps a Clojure function in a way that supports Code objects and partial application
  through argument reordering."
  [sym]
  `(do (deftype type# []
         Object
         (toString [this#]
           (str "Wrapped<" ~sym ">"))
         IInvokable
         (invoke [f# env# args#]
           (if (not-any? code? args#)
             (apply ~sym args#)
             (let [codes# (filter code? args#)
                   not-codes# (remove code? args#)]
               (lift (list* (quote ~sym) (apply ~sym not-codes#) (map reify codes#)))))))
       (register (quote ~sym) (new type#))))


(defmacro register-reorder-unsafe-fn
  [sym]
  `(do (deftype type# []
         Object
         (toString [this#]
           (str "Wrapped<" ~sym ">"))
         IInvokable
         (invoke [f# env# args#]
           (if (not-any? code? args#)
             (apply ~sym args#)
             (let [partitioned# (partition-by code? args#)]
               (lift (cons (quote ~sym)
                           (mapcat (fn [vals#]
                                     (if (code? (first vals#))
                                       (map reify vals#)
                                       [(apply ~sym vals#)]))
                                   partitioned#)))))))
       (register (quote ~sym) (new type#))))

(defmacro register-clj-fn
  [sym]
  `(do (deftype type# []
         Object
         (toString [this#]
           (str "Wrapped<" ~sym ">"))
         IInvokable
         (invoke [f# env# args#]
           (if (not-any? code? args#)
             (apply ~sym args#)
             (lift (cons (quote ~sym)
                         (map (fn [arg#]
                                (if (code? arg#)
                                  (reify arg#)
                                  (quotev arg#)))
                              args#))))))
       (register (quote ~sym) (new type#))))

(defmacro register-side-effect
  [sym]
  `(do (deftype type# []
         Object
         (toString [this#]
           (str "Wrapped<" ~sym ">"))
         IEffect
         (invoke-effect [f# env# args#]
           (apply ~sym args#))
         IInvokable
         (invoke [f# env# args#]
           (if (not-any? code? args#)
             (invoke handler env# (cons f# args#))
             (lift (cons (quote ~sym)
                         (map reify args#))))))
       (register (quote ~sym) (new type#))))


(deftype CodeHandler []
  IInvokable
  (invoke [this env args]
    (lift args)))

(declare ->Lambda)

(deftype Lambda [name start-env arg-syms body]
  Object
  (toString [this]
    (format "(λ %s %s %s)" name arg-syms body))
  IInvokable
  (invoke [this _ args]
    (let [has-code? (volatile! false)
          new-env (assoc start-env name this)
          new-env (into new-env
                        (map
                          (fn [sym arg]
                            (if (code? arg)
                              (do
                                (vreset! has-code? true)
                                [sym (->Code sym)])
                              [sym arg]))
                          arg-syms
                          args))
          sym (if @has-code?
                (let [key [name start-env (mapv (fn [v]
                                                  (if (code? v)
                                                    '_
                                                    v))
                                                args)]]
                  (println "Applying Fn - " name args)
                  (if-let [sym (get @code-cache key)]
                    sym
                    (let [sym (gensym "code")]
                      (swap! code-cache assoc key sym)
                      (let [applied (binding [handler (->CodeHandler)]
                                      (eval-notail new-env body))]
                        (swap! globals assoc sym
                               (->Lambda (symbol (str name "_" sym))
                                         {}
                                         (filterv identity
                                                  (map (fn [sym val]
                                                         (when (code? val)
                                                           sym))
                                                       arg-syms args))
                                         (reify applied)))
                        (println "Applied Key " key " -> " applied)
                        sym)))))
          result (if @has-code?
                   (lift (list* (list 'with-env sym) (map reify (filter code? args))))
                   (eval new-env body))]
      result)))

(defmethod print-method Lambda
  [c ^Writer w]
  (.write w (str c)))

(deftype MakeLambda []
  Fexpr
  IInvokable
  (invoke [f env [name args body]]
    (let [new-env (assoc env name (->Lambda name env args body))
          new-env (into new-env
                        (zipmap args (map lift args)))
          applied (binding [handler (->CodeHandler)]
                    (eval-notail new-env
                                 body))]
      (->Lambda name env args (reify applied)))))

(register 'fn (->MakeLambda))

(deftype Vau [name start-env arg-syms body]
  Object
  (toString [this]
    (format "(vau %s %s %s)" name arg-syms body))
  Fexpr
  IInvokable
  (invoke [f env args]
    (let [new-env (into start-env
                        (zipmap arg-syms [env args]))]
      (eval new-env body))))


(deftype MakeVau []
  Fexpr
  IInvokable
  (invoke [f env [name args body]]
    (let [new-env (into env
                        (zipmap args (map lift args)))
          applied (binding [handler (->CodeHandler)]
                    (eval-notail new-env
                                 body))]
      (->Vau name env args body))))

(register 'vau (->MakeVau))

(deftype Reify []
  IInvokable
  (invoke [f env [v]]
    (reify v)))

(register 'reify (->Reify))

(deftype Eval []
  IInvokable
  (invoke [f env [given-env form :as args]]
    (case (count args)
      1 (eval env given-env)
      2 (if (not-any? code? args)
          (eval given-env form)
          (lift `(eval ~(reify given-env) ~(reify form)))))))

(register 'eval (->Eval))

(deftype Apply []
  IInvokable
  (invoke [_ env [f & rest]]
    (invoke f env (concat (butlast rest) (last rest)))))

(register 'apply (->Apply))

(deftype Quote []
  Fexpr
  IInvokable
  (invoke [f env [val]]
    val))

(register 'quote (->Quote))

(deftype WithEnv []
  IInvokable
  (invoke [_ env [^Lambda f]]
    (->Lambda (.-name f) env (.-arg_syms f) (.-body f))))

(register 'with-env (->WithEnv))

(deftype Let []
  Fexpr
  IInvokable
  (invoke [_ env [binds & body]]
    (loop [env env
           codes []
           [bind expr & more :as remain] binds]
      (if (seq remain)
        (let [result (eval-notail env expr)]
          (if (code? result)
            (recur (assoc env bind (lift bind))
                   (into codes [bind (reify result)])
                   more)
            (recur (assoc env bind result)
                   codes
                   more)))
        (if (seq codes)
          (let [result (eval-notail env (cons 'do body))]
            (lift `(~'let [~@codes]
                     ~(reify result))))
          (eval env (cons 'do body)))))))

(register 'let (->Let))

(deftype ExtendEnv []
  IInvokable
  (invoke [_ env [env sym val]]
    (if (or (code? env)
            (code? sym))
      (lift `(extend-env ~(reify env) ~(reify sym) ~(reify val)))
      (assoc env sym val))))

(register 'extend-env (->ExtendEnv))

(register-clj-fn first)
(register-clj-fn next)
(register-clj-fn second)


(register-clj-fn get)

(register-clj-fn cons)
(register-clj-fn assoc)

(register-reorder-safe-fn +)
(register-reorder-safe-fn -)
(register-reorder-safe-fn *)
(register-reorder-unsafe-fn /)

(register-clj-fn seq)
(register-clj-fn concat)
(register-clj-fn list)
(register-clj-fn vector)

(register-clj-fn <)
(register-clj-fn >)

(register-clj-fn =)
(register-clj-fn nth)
(register-clj-fn count)
(register-clj-fn dec)
(register-clj-fn inc)
(register-clj-fn char)

(register-side-effect println)
(register-side-effect print)

(defn load-file [file]
  (let [data (read-string (str "[" (slurp file) "]"))]
    (doseq [form data]
      (eval-notail form))))

(let [old-globals @globals
      old-code-cache @code-cache]
  (defn reset-globals []
    (reset! globals old-globals)
    (reset! code-cache old-code-cache)))

(defn load-stdlib []
  (load-file "stdlib.heli"))


(comment

  (load-file "src/lisp_in_x/stdlib.fae")

  @globals

  (eval-notail '(def s (if (lift 4) 3 4)))

  (eval-notail '(+ 1 2 (/ 1 2 (lift 3) 4 5) 4 5))

  (eval-notail '(< (lift 1) 2 3))

  (eval-notail '((fn test [x] (+ x 1)) 3))

  (eval-notail '(do (def count-up (fn count-up [x max]
                                    (if (< x max)
                                      (count-up (+ x 1) max)
                                      max)))
                    (eval (reify (count-up (lift 0) 10)))))

  (eval-notail '(do (def count-up (fn count-up [x max]
                                    (if (< x max)
                                      (count-up (+ x 1) max)
                                      max)))
                    (eval (reify (count-up (lift 0) 10)))))

  (eval-notail '(do (def count-up (fn count-up [x max]
                                    (if (< x max)
                                      (count-up (+ x 1) max)
                                      max)))
                    (count-up 0 10)))

  (eval-notail '(do (def get-in (fn get-in [acc path]
                                  (do
                                    (println acc path (get acc (next path)))
                                    (if path
                                      (get-in (get acc (first path)) (next path))
                                      acc))))

                    (get-in (lift {:a {:b 3}}) [:a :b])))

  (eval-notail '(do (def outer (fn outer [max]
                                 (fn inner [x]
                                   (+ x max))))
                    ((outer 10) (lift 3))))

  (eval-notail '(do (def when (vau when [env args]
                                   (if (eval env (first args))
                                     (eval env (cons 'do (next args)))
                                     nil)))
                    (def main (fn main [x]
                                (when x
                                  (lift (println "hey"))
                                  42)))


                    (fn main [x]
                      (when x
                        (println "Sup?")
                        4))

                    #_(main (lift true))))

  (eval-notail '(do (def let (vau let [env args]
                                  (eval (assoc env (first (first args))
                                                   (eval env (second (first args))))
                                        (cons 'do (next args)))))

                    (def let (vau let [env args]
                                  (eval )
                                  (eval 'do (next args))))
                    (def main (fn main [x]
                                (let [y (+ x 1)]
                                  y)))



                    (main 4)))

  (eval-notail '(do (def let (vau let [env args]
                                  (eval env `((~'fn ~'let-inner [~(first (first args))]
                                                (do ~@(next args)))
                                               ~(second (first args))))))
                    (def do-it (fn do-it [x y]
                                 (let [z x]
                                   (+ x y z))))
                    (eval (reify (do-it (lift 4) 1)))))

  (eval-notail '(do (def func (fn func [x y]
                                (do (println "X: " x)
                                    (println "Y: " y)
                                    (+ x y))))
                    (reify (func 1 (lift 3)))))

  (eval-notail '(code8274 3))

  (eval-notail '(apply vector [1 2 3]))


  (eval-notail '(let [x 2
                      y 3]
                  (+ x y)))

  (eval-notail '(fn f [x] x))


  @globals
  @code-cache


  (source get-in)

  )

