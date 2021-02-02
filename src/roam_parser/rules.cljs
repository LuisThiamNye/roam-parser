(ns roam-parser.rules
  (:require
   [clojure.string]
   [taoensso.timbre :as t]
   [roam-parser.transformations :as transf]
   [roam-parser.elements :as elements]
   [roam-parser.utils :as utils]
   [roam-parser.rules.page :refer [start-page-link]]
   [roam-parser.rules.block-beginning :refer [block-beginning-rules]]
   [roam-parser.rules.alias :refer [start-alias-square start-image-square]]
   [roam-parser.rules.render :refer [start-render]]
   [roam-parser.rules.formatting :refer [start-formatting]]))



(def escapable-char-regex #"[\\{}\[\]\(\)`\*_\^:#!\n>]|\${2}")
(def codeblock-re (js/RegExp #"^(?:(javascript|css|html|clojure|common lisp)\n|\n?)(.*(?:[^\n]|$))" "s"))
(def default-codeblock-lang "javascript")



(defn skip-escape-char [state char]
  (when (identical? \\ char)
    #(update % :idx + 2)))



(defn text-rules [state char]
  (let [rules (-> state :path peek :context/text-rules)]
    (loop [i (dec (count rules))]
      (when-not (neg? i)
        (or ((nth rules i) state char)
            (recur (dec i)))))))

;; processed from end to beginning. Order of descending priority
(def rules [text-rules
            skip-escape-char
            start-render
            start-formatting
            start-page-link
            start-image-square
            start-alias-square
            block-beginning-rules])

;;
;; comment
;;

(comment

  (def ex-s "the somethign that happened the other day is reallly troubling me and my \\$$faamilay ow aiohofpdd")
  (def re (js/RegExp. escapable-char-regex))
  (simple-benchmark [] (.exec re ex-s) (* 1000 7))
  ;; 4
  (simple-benchmark [] (clojure.string/join (seq ex-s)) (* 1000 7))
  ;; 88
  (simple-benchmark [] (loop [i (dec (count ex-s))]
                         (when-not (neg? i)
                           (nth ex-s i)
                           (recur (dec i)))) (* 1000 7))
  ;; 37
  ;; conclusion: regex is really fast

  ;;
  (simple-benchmark [] (name :context.id/some-id) 10000)
  ;; 3
  (simple-benchmark [x {:get :some-id}] (:get x) 10000)
  ;; 6
  (simple-benchmark [x :b] (case x :a 8 :b 7 :c 4 :d 2) 10000)
  ;; 3
  (simple-benchmark [x :context.id/some-id] (case (name x)
                                              "somdkf" 4
                                              "dsdfsd" 7
                                              "some-id" 8
                                              "else" 6) 10000)
  ;; 3
  (simple-benchmark [] (contains? nil :this-that) 10000)
;; 3
  (simple-benchmark [] (contains? #{} :this-that) 10000)
;; 3

  (simple-benchmark [] (identical? "a" " ") 100000)
  ;; 1

  (simple-benchmark [] (re-find #"\S" " ") 100000)
  ;; 5
  (simple-benchmark [] (re-find #"\S" "a") 100000)
  ;; 8

  (simple-benchmark [] (identical? 3 3) 10000)
  ;; 0
  (simple-benchmark [] (= 3 3) 10000)
  ;; 6

  ;; end
  )
