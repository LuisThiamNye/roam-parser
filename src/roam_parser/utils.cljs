(ns roam-parser.utils)

(defn eager-filter [f coll]
  (reduce #(if (f %2) (conj % %2) %) [] coll))

(defn re-to-str [re] (.slice (str re) 1 -1))
