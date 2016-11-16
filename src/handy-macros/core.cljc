(ns handy-macros.core)

;; (let [jsobj #js {"a" 1 "b" 2}]
;;  (destruct-js jsobj [a b]
;;    (console.log a b))

(defmacro destruct-js [obj keys & body]
  (let [bindings
        (->> keys
         (map name)
         (mapcat (fn [k] [(symbol k) `(.. ~obj ~(symbol (str "-" k)))]))
         (into []))]
    `(let ~bindings
      ~@body)))
