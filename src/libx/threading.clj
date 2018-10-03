(ns libx.threading)

(def known-transforms
  #{#'clojure.core/map
    #'clojure.core/mapv
    #'clojure.core/filter
    #'clojure.core/filterv})

(defn sym->list [x]
  (cond
    (list? x) x
    (symbol? x) (list x)
    :else [x x]))

(defn filterv-mapv [f m xs]
  `(let [m# ~m
         f# ~f]
     (loop [[x# & xs#] ~xs
            ret# []]
       (let [ret# (if (f# x#) (conj ret# (m# x#)) ret#)]
         (if-not (empty? xs#)
           (recur xs# ret#)
           ret#)))))

(defn mapv-filterv [m f xs]
  `(let [m# ~m
         f# ~f]
     (loop [[x# & xs#] ~xs
            ret# []]
       (let [x#   (m# x#)
             ret# (if (f# x#) (conj ret# x#) ret#)]
         (if-not (empty? xs#)
           (recur xs# ret#)
           ret#)))))

(defn map-filter [m f xs]
  `(seq ~(mapv-filterv m f xs)))

(defn filter-map [m f xs]
  `(seq ~(filterv-mapv m f xs)))

(defn fuse-map* [f forms]
  [(list f `(fn [x#] (clojure.core/->> x# ~@(map #(list (second %)) forms))))])

(defn fuse-filter* [f forms]
  (let [arg   (gensym)
        forms (map second forms)
        forms (map (fn [x] (list x arg)) forms)]
    [(list f `(fn [~arg] (and ~@forms)))]))

(defn ->comp [forms]
  (let [f (ffirst forms)]
    (cond
      (not (list? (first forms))) forms
      (list? f) forms
      :else (let [f' (resolve f)]
              (cond
                (= 1 (count forms)) forms

                (contains? known-transforms f')
                (case f
                  map (fuse-map* 'map forms)
                  mapv (fuse-map* 'mapv forms)
                  map-indexed (fuse-map* 'map-indexed forms)
                  mapcat (fuse-map* 'mapcat forms)
                  filter (fuse-filter* 'filter forms)
                  filterv (fuse-filter* 'filterv forms)
                  forms)

                :else forms)))))

(defn -update-ret-type [ret type f]
  (conj (pop ret) (concat [type] (rest (last ret)) [f])))

(defn -group-transforms [forms]
  (loop [[f & fs] forms
         ret [[]]]
    (cond
      (and (nil? f) (empty? fs))
      ret

      (empty? (last ret))
      (recur fs (conj (pop ret) (conj (last ret) (first f) f)))

      :else
      (let [f' (first (last ret))]
        (cond
          (and (= (first f) 'map) (= f' 'filter))
          (recur fs (-update-ret-type ret 'filter-map f))

          (and (= (first f) 'filter) (= f' 'map))
          (recur fs (-update-ret-type ret 'map-filter f))

          (and (= (first f) 'mapv) (= f' 'filterv))
          (recur fs (-update-ret-type ret 'filterv-mapv f))

          (and (= (first f) 'filterv) (= f' 'mapv))
          (recur fs (-update-ret-type ret 'mapv-filterv f))

          :else (recur fs (conj ret [(first f) f])))))))

(defn -fuse-map<->filter [[t & fs]]
  (let [xs (gensym)]
    (case t
      map-filter
      (let [[m f] fs]
        `((fn [~xs] ~(map-filter (second m) (second f) xs))))

      mapv-filterv
      (let [[m f] fs]
        `((fn [~xs] ~(mapv-filterv (second m) (second f) xs))))

      filter-map
      (let [[f m] fs]
        `((fn [~xs] ~(filter-map (second f) (second m) xs))))

      filterv-mapv
      (let [[m f] fs]
        `((fn [~xs] ~(filterv-mapv (second m) (second f) xs))))

      (first fs))))

(defn fuse-map<->filter [forms]
  (clojure.core/->> forms
                    -group-transforms
                    (map -fuse-map<->filter)
                    (map #(let [[x] %]
                            (if (list? %)
                              %
                              x)))))

(defn fuse-transforms [forms]
  (clojure.core/->> forms
                    (map sym->list)
                    (partition-by first)
                    (mapcat ->comp)
                    fuse-map<->filter))
