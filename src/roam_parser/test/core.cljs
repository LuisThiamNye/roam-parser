(ns roam-parser.test.core (:require [clojure.string :as cstr]
                                    [roam-parser.core :as parser]
                                    [roam-parser.test.sample :as sample]))

(def lines (cstr/split sample/text #"\n[ \n]*"))

(def test-str-f "There are_ some__ **bug*s**** in our _parser^ - it__ doesn't handle certain kinds of^^ nesting - and it __is ^^^a bi__t of a pain to ex_tend -- also - it *is *painful whenever we want t^__^^^o make up more speci^^fic syntax - like for queries.")

(def test-str "There are some bugs in our parser - it doesn't handle certain kinds of nesting - and it is a bit of a pain to extend -- also - it is painful whenever we want to make up more specific syntax - like for queries.")

(defn test-2 []
  (time (doall (for [i (range 1000)] (parser/parse-inline test-str-f))))
  nil)

(set! (.-test2 js/window) test-2)
(set! (.-parc js/window) parser/parse-inline)
(set! (.-parb js/window) #(do (parser/parse-inline %) nil))


(defn build-real-results [start end]
  (reduce #(conj % (parser/parse-inline %2)) [] (subvec lines start end)))

(defn real-test-builder
  ([start end]
   (time (doseq [line (subvec lines start end)]
           (parser/parse-inline line)))
   nil)
  ([start end debug]
   (if debug
     (time (doseq [line (subvec lines start end)]
             (parser/parse-inline (parser/probe line))))
     (real-test-builder start end))
   nil))

(set! (.-real js/window) real-test-builder)


(defn restring [text]
  (parser/stringify-block {:children (parser/parse-inline text)}))

(defn real-test-str
  ([start end]
   (let [bs (build-real-results start end)]
     (time (doseq [b bs]
             (parser/stringify-block b))))
   nil)
  ([start end debug]
   (if debug
     (let [bs (build-real-results start end)]
       (time (doseq [b bs]
               (parser/stringify-block (parser/probe b)))))
     (real-test-str start end))
   nil))

(set! (.-sfy js/window) restring)
(set! (.-realstr js/window) real-test-str)
