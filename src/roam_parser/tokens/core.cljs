(ns roam-parser.tokens.core
  (:require
   [roam-parser.elements :as el]
            [roam-parser.utils :as utils]
            [roam-parser.rules :as rules]
            [roam-parser.tokens.token :as token]))




;; (defn token-from-text [^string text idx]
;;   (case (nth text 0)
;;     ")" (token/->Round idx (count text) :close)
;;     "(" (token/->Round idx (count text) :open)
;;     "]" (token/->Square idx (count text) :close)
;;     "[" (token/->Square idx (count text) :open)
;;     "{" (token/->Curly idx (count text) :open)
;;     "}" (token/->Curly idx (count text) :close)
;;     ":" (token/->Attribute idx (count text))
;;     "`" (token/->Backtick idx (count text))
;;     "!" (token/->Bang idx)
;;     "#" (token/->Hash idx)
;;     "-" (token/->Hr idx)
;;     \\ nil
;;     nil))

;; (def escape-seq-regex (str "\\\\(?:" (utils/re-to-str rules/escapable-char-regex) ")"))


;; (defn to-regex [^string str] (js/RegExp. str "gm"))
;; TODO, use interpose?
;; (def inline-re (to-regex (reduce #(str %1 "|" (utils/re-to-str %2 ))
;;                                  escape-seq-regex regex-pieces)))
