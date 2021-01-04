(ns roam-parser.dev.core
  (:require [clojure.string :as cstr]
            [cljs.test :as t :include-macros true]
                                   [devcards.core :as dc :include-macros true]
                                    [roam-parser.core :as parser]
                                    [roam-parser.dev.sample :as sample]
                                    [roam-parser.elements :as el]
                                    [roam-parser.utils :as utils])
    (:require-macros [devcards.core :refer [defcard deftest]]))

(dc/start-devcard-ui!)

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
             (parser/parse-inline (utils/probe line))))
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
               (parser/stringify-block (utils/probe b)))))
     (real-test-str start end))
   nil))

(set! (.-sfy js/window) restring)
(set! (.-realstr js/window) real-test-str)

(def strs [["[[This [[is]]/**not**/ ![ready](url)]] " "[[This [[is]]/**not**/ ![ready](url)]] "]
           ["```clojure\ngood```" "```clojure\ngood\n```"]])

(defn ts [i] (let [i i](t/is (= (parser/parse-inline (nth strs i))
                           (nth strs i)))))

(deftest str-tests
  (doall (for [[i o] strs] (t/is (= (el/stringify (parser/parse-inline i))
                                o)))))

(defcard lotsof
  "`[[This [[is]]/**not**/ ![ready](url)]] `"
  (parser/parse-inline "[[This [[is]]/**not**/ ![ready](url)]] "))

(defcard some-tests
  "`[[This [[is]]]]`"
  (parser/parse-inline "[[This [[is]]]] "))

(defcard image
  "`! [ an image ] ( url )`"
  (parser/parse-inline "![an image](url)"))

(defcard alias-page
  "`! [ an image ] ( url )`"
  (parser/parse-inline "[an]([[page [[name]]]])"))

(defcard alias-bref
  "`! [ an image ] ( url )`"
  (parser/parse-inline "[an](((bref)))"))

(defcard no-alias-bref
  "`! [ an image ] ( url )`"
  (parser/parse-inline "[an]((bref))"))
