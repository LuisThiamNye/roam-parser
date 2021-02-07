(ns roam-parser.utils
  (:require
   [clojure.string]))

(defn probe
  ([x] (.log js/console x) x)
  ([n x] (.log js/console n x) x))

(defn eager-filter [f coll]
  (reduce #(if (f %2) (conj %1 %2) %1) [] coll))

(defn re-to-str [re] (.slice (str re) 1 -1))

(defn assoc-last [coll item]
  (assoc coll (dec (count coll)) item))

(defn update-last [coll f]
  (assoc-last coll (f (peek coll))))

(defn no-blank-ends? [s]
  (not (or (nil? s)
           (identical? s "")
           (not  (identical? s (clojure.string/trim s))))))

(defn str->float [^string string]
  (let [result (js/parseFloat string)]
    (if (js/isNaN result) nil result)))
