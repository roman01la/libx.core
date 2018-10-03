(ns libx.core
  (:refer-clojure :exclude [->> for doseq let])
  (:require [libx.threading :refer [fuse-transforms]]
            [clojure.core :as cc]))


(defmacro let
  "Works as clojure.core/let, but additionally can destructure JavaScript objects via :goog.object/keys form"
  [bindings & body]
  (cc/let [bindings' (cc/->> bindings
                             (partition 2)
                             (filter #(map? (first %)))
                             (filter #(-> (first %) (contains? :goog.object/keys)))
                             (map (fn [[{ks :goog.object/keys} v]]
                                    (cc/let [vs (gensym "value_")
                                             bs (cc/->> (repeat (count ks) vs)
                                                        (interleave ks)
                                                        (partition 2)
                                                        (mapcat (fn [[k v]]
                                                                  [k `(goog.object/get ~v ~(str k))])))]
                                      (into [vs v] bs))))
                             (into []))

           bindings' (loop [[[b v] & bs] (partition 2 bindings)
                            bs' bindings'
                            ret []]
                       (cond
                         (and (empty? bs') (seq bs))
                         (recur bs bs' (conj ret b v))

                         (and (empty? bs') (empty? bs) b v)
                         (conj ret b v)

                         (and (empty? bs') (empty? bs))
                         ret

                         (and (map? b) (contains? b :goog.object/keys))
                         (recur bs (rest bs') (into [] (concat ret (first bs'))))

                         :else (recur bs bs' (conj ret b v))))]

    `(cc/let ~bindings' ~@body)))

;; (if-keys [{:keys [a b]} {:a 1 :b 2}]
;;   (println a b)
;;   (println "nothing"))
(defmacro if-keys
  ([bindings then]
   `(if-keys ~bindings ~then nil))
  ([bindings then else]
   (let [forms  (:keys (bindings 0))
         tst    (bindings 1)
         nbinds (cc/->> forms
                        (map (fn [s] `(~(keyword s) ~tst)))
                        (interleave forms)
                        (into []))]
     `(let ~nbinds
        (if (and ~@forms)
          ~then
          ~else)))))

;; (when-keys [{:keys [a b]} {:a 1 :b 2}]
;;   (println a b))
(defmacro when-keys
  "When all symbols in destructuring form are evaluated to true,
  evaluates body with those symbols bound to corresponding values in a map"
  [bindings & body]
  `(if-keys ~bindings (do ~@body)))

(defn -rewrite-for [bindings body]
  (let [[item coll] bindings]
    `(lazy-seq
       (loop [[~item & xs#] ~coll
              ret# []]
         (let [next# (conj ret# ~body)]
           (if (seq xs#)
             (recur xs# next#)
             (seq next#)))))))

(defmacro for
  "Rewrites simple form `for` into `loop`"
  [seq-exprs body-expr]
  (if (= 2 (count seq-exprs))
    (-rewrite-for seq-exprs body-expr)
    `(cc/for ~seq-exprs ~body-expr)))

(defn -rewrite-doseq [bindings body]
  (let [[item coll] bindings]
    `(let [coll# ~coll]
       (when-not (empty? coll#)
         (loop [[~item & xs#] coll#]
           ~@body
           (when (seq xs#)
             (recur xs#)))))))

(defmacro doseq
  "Rewrites simple form `doseq` into `loop`"
  [seq-exprs & body]
  (if (= 2 (count seq-exprs))
    (-rewrite-doseq seq-exprs body)
    `(cc/doseq ~seq-exprs ~@body)))

(defmacro ->>
  "Combines collections transforms (map, filter, etc.) to eliminate intermediate collections"
  [x & forms]
  `(cc/->> ~x ~@(fuse-transforms forms)))
