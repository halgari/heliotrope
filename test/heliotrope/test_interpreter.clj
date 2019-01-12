(ns heliotrope.test-interpreter
  (:require [clojure.test :refer :all]
            [heliotrope.interpreter :as i]
            [clojure.walk :as walk])
  (:refer-clojure :exclude [eval])
  (:import (heliotrope.interpreter Lambda)))


(defmacro defitest [name & body]
  `(deftest ~name
     (i/reset-globals)
     (i/load-stdlib)
     ~@body))

(defn clean-data [data]
  (walk/postwalk
    (fn [x]
      (if (qualified-symbol? x)
        (symbol (name x))
        x))
    data))

(defn eval [form]
  (i/eval-notail (clean-data form)))

(defn ^Lambda eval-fn [form]
  (i/eval-notail (clean-data form)))

(defn eval-reify [form]
  (i/reify (i/eval-notail (clean-data form))))

(defitest test-constants
  (is (= (eval 1) 1))
  (is (= (eval "test") "test"))
  (is (= (eval [1 2]) [1 2])))

(defitest test-if
  (is (= (eval '(if true 1 2)) 1))
  (is (= (eval '(if false 1 2)) 2))

  (is (= (eval-reify '(if (lift true) 1 2)) '(if true 1 2)))
  (is (= (eval-reify '(if true 1 2)) 1)))

(defitest test-do
  (is (= (eval '(do 1)) 1))
  (is (= (eval '(do 1 2)) 2))
  (is (= (eval-reify '(do (lift 1) 2)) '(do 1 2)))
  (is (= (eval-reify '(do (lift 1) 2 3)) '(do 1 3))))


(defitest test-when
  (is (= (.-body (eval-fn '(fn foo [x]
                             (when x
                               4))))
         '(if x 4 nil))))

(defitest test-cond
  (is (= (eval '(cond
                  1 "1"
                  2 "2")) "1"))
  (is (= (eval '(cond
                  false "1"
                  2 "2"))
         "2")))

(defitest test-destructuring
  (is (= (eval '(do (defvau with-foo [env body]
                            (eval (extend-env env 'foo 42)
                                  (first body)))
                    (with-foo
                      foo)))
         42))

  (is (= (.-body (eval-fn '(do (defvau with-foo [env body]
                                       (eval (extend-env env 'foo 42)
                                             (first body)))
                               (fn some-fn [x]
                                 (with-foo
                                   (+ foo x))))))
         '(+ 42 x)))

  (is (= (.-body (eval-fn '(do (defvau let-kv [env body]
                                       (let [bindings (first body)
                                             syms (first bindings)
                                             result (eval env (second bindings))
                                             a (first result)
                                             b (second result)
                                             new-env (extend-env env (first syms) a)
                                             new-env (extend-env new-env (second syms) b)]
                                         (eval new-env (second body))))

                               (fn add-them [v]
                                 (let-kv [[x y] v]
                                         (+ 1 x y))))))
         '(let
            [result v
             a (first result)
             b (second result)]
            (+ 1 a b)))))