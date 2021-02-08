(ns roam-parser.dev.core
  (:require
   [clojure.string :as cstr]
   [taoensso.timbre :as timbre]
   [cljs.test :as t :include-macros true]
   [devcards.core :as dc :include-macros true :refer-macros [deftest defcard]]
   [roam-parser.core :as parser]
   [roam-parser.dev.sample :as sample]
   [roam-parser.elements :as el]
   [roam-parser.utils :as utils]))

(dc/start-devcard-ui!)

(def lines (cstr/split sample/text #"\n[ \n]*"))


(defn build-real-results [start end]
  (reduce #(conj % (parser/parse-block %2)) [] (subvec lines start end)))

(defn evade-logs [f]
  (timbre/set-level! :info)
  (f)
  (timbre/set-level! :debug))

(defn real-test-builder
  ([start end]
   (evade-logs #(time (doseq [line (subvec lines start end)]
                        (parser/parse-block line))))
   nil)
  ([start end debug]
   (evade-logs #(if debug
                  (time (doseq [line (subvec lines start end)]
                          (parser/parse-block (utils/probe line))))
                  (real-test-builder start end)))
   nil))

(defn restring [text]
  (el/stringify (parser/parse-block text)))

(defn real-test-str
  ([start end]
   (evade-logs #(let [bs (build-real-results start end)]
                  (time (doseq [b bs]
                          (el/stringify b)))))
   nil)
  ([start end debug]
   (evade-logs #(if debug
      (let [bs (build-real-results start end)]
        (time (doseq [b bs]
                (el/stringify (utils/probe b)))))
      (real-test-str start end)))
   nil))

(set! (.-block js/window) parser/parse-block)
(set! (.-help js/window) real-test-builder)
(set! (.-str js/window) restring)
(set! (.-helpstr js/window) real-test-str)

(def strs [["[[This [[is]]/**not**/ ![ready](url)]] " "[[This [[is]]/**not**/ ![ready](url)]] "]
           ["```clojure\ngood```" "```clojure\ngood\n```"]
           ["{{Δ:4+7  }}" "{{Δ: 4+7}}"]])

(defn ts [i] (let [i i] (t/is (= (parser/parse-block (nth strs i))
                                 (nth strs i)))))

(deftest str-tests
  (doall (for [[i o] strs] (t/is (= (el/stringify (parser/parse-block i))
                                    o)))))

(defcard bob
  "`[[This [[is]]/**not**/ ![egg](url)]] `
   (image invalidates larger page)"
  (parser/parse-block "[[This [[is]]/**not**/ ![egg](url)]] "))

(defcard
  "`[an]([[page [[name]]]])`"
  (parser/parse-block "[an]([[page [[name]]]])"))

(defcard
  "`[an](((bref)))`"
  (parser/parse-block "[an](((bref)))"))

(defcard
  "```
[an]((bref))
```"
  (parser/parse-block "[an]((bref))"))

(defcard
  "```
((parenthetical #tag #[[page]] ))
```"
  (parser/parse-block "((parenthetical #tag #[[page]] ))"))
(defcard
  "```
:hiccup code
```"
  (parser/parse-block ":hiccup code"))
(defcard
  "```
(visit www.example.com) and https://example.com/?)
```"
  (parser/parse-block "(visit www.example.com) and https://example.com/?)"))
(defcard
  "```
[[>]] $$latex$$
```"
  (parser/parse-block "[[>]] $$latex$$"))
(defcard
  "```
[[[[other]] attribute]]::
```"
  (parser/parse-block "[[[[other]] attribute]]::"))
(defcard
  "```
an attribute::
```"
  (parser/parse-block "an attribute::"))
(defcard
  "```
{{∆: {some {content}}}}
```"
  (parser/parse-block "{{∆: {some {content}}}}"))
(defcard
  "```
{{∆: }}
```"
  (parser/parse-block "{{∆: }}"))
(defcard
  "```
{{∆}}
```"
  (parser/parse-block "{{∆}}"))
(defcard
  "```
**__^^all^^ the__ formatting**
```"
  (parser/parse-block "**__^^all^^ the__ formatting**"))
(defcard
  "```
backtick: ```
```"
  (parser/parse-block "backtick: ```"))
(defcard
  "```
`co\\`de`
```"
  (parser/parse-block "`co\\`de`"))
(defcard
  "```
`` `clojure\\ncode```
```"
  (parser/parse-block "```clojure\ncode```"))
(defcard
  "```
[[nested [[pages]]]]
```"
  (parser/parse-block "[[nested [[pages]]]]"))
(defcard
  "```
[![alias](inside)](alias)
```"
  (parser/parse-block "[![alias](inside)](alias)"))
(defcard
  "```
[[ escaping \\[\\[  ]]
```"
  (parser/parse-block "[[ escaping \\[\\[  ]]"))
