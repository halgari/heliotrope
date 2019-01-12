# heliotrope
A lisp, written in Clojure, designed for exploring how languages like [Pink](https://github.com/namin/pink) can be extended with
features of other lisps. So far these extensions include multi-argument functions (vs curried functions), and fexprs. Fexprs can
be viewed as a mini-interpreter that ingests sexprs and interprets them. Using the concepts described in this [paper] 
(http://lampwww.epfl.ch/~amin/pub/collapsing-towers.pdf) the overhead of fexprs can be removed during runtime. The tests show an
example of this behavior: 

```clojure
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
            (+ 1 a b))))

```


Note: this repository is mostly a place for me to hack on new interpreter ideas. The code is ugly and highly inefficient. 
