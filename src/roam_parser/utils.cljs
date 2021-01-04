(ns roam-parser.utils)

(defn probe
  ([x] (.log js/console x) x)
  ([n x] (.log js/console n x) x))

(defn eager-filter [f coll]
  (reduce #(if (f %2) (conj %1 %2) %1) [] coll))

(defn eager-map [f coll]
  (reduce #(conj %1 (f %2)) [] coll))

(defn re-to-str [re] (.slice (str re) 1 -1))
