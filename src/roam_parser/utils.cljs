(ns roam-parser.utils)

(defn eager-filter [f coll]
  (reduce #(if (f %2) (conj % %2) %) [] coll))
