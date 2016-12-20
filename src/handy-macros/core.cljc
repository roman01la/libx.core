(ns handy-macros.core)

;; (let [jsobj #js {"a" 1 "b" 2}]
;;  (destruct-js jsobj [a b]
;;    (console.log a b))
(defmacro destruct-js
  "Destructuring syntax for JavaScript objects"
  [obj keys & body]
  (let [bindings
        (->> keys
         (map name)
         (mapcat (fn [k] [(symbol k) `(.. ~obj ~(symbol (str "-" k)))]))
         (into []))]
    `(let ~bindings
      ~@body)))

;; (when-keys [{:keys [a b]} {:a 1 :b 2}]
;;   (println a b))
(defmacro when-keys
  "When all symbols in destructuring form are evaluated to true,
  evaluates body with those symbols bound to corresponding values in a map"
  [bindings & body]
  (let [forms (:keys (bindings 0))
        tst (bindings 1)
        nbinds (->> forms
                    (map (fn [s] `(~(keyword s) ~tst)))
                    (interleave forms)
                    (into []))]
    `(let ~nbinds
       (when (and ~@forms)
         ~@body))))
