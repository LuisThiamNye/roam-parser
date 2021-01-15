(ns roam-parser.tokens.core (:require [roam-parser.elements :as el]
                                      [roam-parser.utils :as utils]
                                      [roam-parser.rules :as rules]
                                      [roam-parser.tokens.series :refer [CodeblockSequence BacktickSequence CurlySequence HrSequence SquareSequence RoundSequence LatexSequence HighlightSequence BoldSequence ItalicSequence UrlSequence AttributeSequence TagSequence]]
                                      [roam-parser.tokens.token :as token]))

(def t-seq-order [CodeblockSequence BacktickSequence CurlySequence HrSequence SquareSequence RoundSequence LatexSequence HighlightSequence BoldSequence ItalicSequence UrlSequence AttributeSequence TagSequence])

;(def -queue [Codeblock Backtick Curly Hr Square Round Latex Highlight Bold Italic Url Attribute Tag])

(def regex-pieces [
                   ;; hiccup
                   #"(?<!\n)^:hiccup\s*(?<hiccup>(?:.|\n)*)"

                   ;; blockquote
                   #"(?<!\n)^(?<blockquote>(?:>|\[\[>\]\]|#\[\[>\]\])\s?)(?=.)"

                   ;; hr
                   #"^---$"

                   ;; codeblock
                   #"(?<!`)(?:(?<codeblockDual>`{3}(?!.*?`{3}))|(?<codeblockClose>`{3,}))(?!`)"

                   ;; code / backtick
                   #"`+"

                   ;; curly brackets
                   #"\{+|\}+"

                   ;; attribute
                   #"(?<=\S)::"

                   ;; latex / $$
                   #"(?<!\$)(?:(?<latexOpen>(?<!\S)\${2}(?=\S))|(?<latexClose>(?<=\S)\${2}(?!\S))|(?<latexDual>\${2}))(?!\$)"

                   ;; square bracket
                   #"\[+|\]+"

                   ;; round bracket
                   #"\(+|\)+"

                   ;; bold / **
                   #"(?<!\*)(?:(?<boldOpen>(?<!\S)\*{2,}(?=\S))|(?<boldClose>(?<=\S)\*{2,}(?!\S))|(?<boldDual>(?<=\S)\*{2,}(?=\S)))(?!\*)"

                   ;; italic / __
                   #"(?<!_)(?:(?<italicOpen>(?<!\S)_{2,}(?=\S))|(?<italicClose>(?<=\S)_{2,}(?!\S))|(?<italicDual>(?<=\S)_{2,}(?=\S)))(?!_)"

                   ;; highlight / ^^
                   #"(?<!\^)(?:(?<highlightOpen>(?<!\S)\^{2,}(?=\S))|(?<highlightClose>(?<=\S)\^{2,}(?!\S))|(?<highlightDual>(?<=\S)\^{2,}(?=\S)))(?!\^)"

                   ;; tag
                   #"(?<=[ \(\[:]|^)#(?<tag>[\w-_/@:]+(?<!::)(?!:(?!:)))"

                   ;; ref: hash / #
                   #"#(?=\[)"

                   ;; url
                   #"(?<![\w\.-])(?<url>https?://[a-zA-Z\d-\.]+\.[a-zA-Z\d-]{2,}|www\.[a-zA-Z\d-\.]+\.[a-zA-Z\d-]{2,})"

                   ;; ref: bang / !
                   #"!(?=\[)"])

(defn token-from-group [group ^string text idx length]
  (case group
    "tag" (token/->Tag idx length text)
    "url" (token/->Url idx length)

    "codeblockDual" (token/->Codeblock idx length :dual)
    "codeblockClose" (token/->Codeblock idx length :close)

    "latexOpen" (token/->Latex idx :open)
    "latexClose" (token/->Latex idx :close)
    "latexDual" (token/->Latex idx :dual)

    "boldOpen" (token/->Bold idx length :open)
    "boldClose" (token/->Bold idx length :close)
    "boldDual" (token/->Bold idx length :dual)

    "italicOpen" (token/->Italic idx length :open)
    "italicClose" (token/->Italic idx length :close)
    "italicDual" (token/->Italic idx length :dual)

    "highlightOpen" (token/->Highlight idx length :open)
    "highlightClose" (token/->Highlight idx length :close)
    "highlightDual" (token/->Highlight idx length :dual)))

(defn token-from-text [^string text idx]
  (case (nth text 0)
    ")" (token/->Round idx (count text) :close)
    "(" (token/->Round idx (count text) :open)
    "]" (token/->Square idx (count text) :close)
    "[" (token/->Square idx (count text) :open)
    "{" (token/->Curly idx (count text) :open)
    "}" (token/->Curly idx (count text) :close)
    ":" (token/->Attribute idx (count text))
    "`" (token/->Backtick idx (count text))
    "!" (token/->Bang idx)
    "#" (token/->Hash idx)
    "-" (token/->Hr idx)
    \\ nil
    nil))

(def escape-seq-regex (str "\\\\(?:" (utils/re-to-str rules/escapable-char-regex) ")"))


(defn to-regex [^string str] (js/RegExp. str "gm"))
;; TODO, faster to use interpose?
(def inline-re (to-regex (reduce #(str %1 "|" (utils/re-to-str %2 ))
                                 escape-seq-regex regex-pieces)))
