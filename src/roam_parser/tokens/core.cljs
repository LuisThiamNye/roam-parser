(ns roam-parser.tokens.core (:require [roam-parser.elements :as el]
                                     [roam-parser.utils :as utils]
                                     [roam-parser.rules :as rules]
                                     [roam-parser.builder :as builder]))

(def queue [CodeblockSequence BacktickSequence CurlySequence HrSequence SquareSequence RoundSequence LatexSequence HighlightSequence BoldSequence ItalicSequence UrlSequence AttributeSequence TagSequence])

(def delimiters [::hiccup (array-map :length 7)
                 ::blockquote (array-map)
                 ::hr (array-map :flags #{:single}
                            :length 3)
                 ::codeblock (array-map :flags #{:greedy}
                            :length 3)
                 ::backtick (array-map :flags #{:greedy}
                            :length 1)
                 ::curly (array-map :flags #{:bracket}
                            :length 1)
                 ::attribute (array-map  :flags #{:single})
                 ::latex (array-map :length 2)
                 ::square (array-map :flags #{:bracket}
                            :length 1)
                 ::round (array-map  :flags #{:bracket}
                             :length 1)
                 ::bold (array-map :flags #{:greedy}
                            :length 2)
                 ::italic (array-map :flags #{:greedy}
                            :length 2)
                 ::highlight (array-map :flags #{:greedy}
                            :length 2)
                 ::tag (array-map :flags #{:single})
                 ::hash (array-map :length 1)
                 ::url (array-map :flags #{:single})
                 ::bang (array-map  :length 1)])

(def regex-pieces [
                   ;; hiccup
                   #"(?<!\n)^:hiccup\s*(?<hiccup>(?:.|\n)*)"

                   ;; blockquote
                   #"(?<!\n)^(?<blockquote>>|\[\[>\]\]|#\[\[>\]\])(?=.)"

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

(defn token-from-group [group]
  (case group
    "blockquote"
    "tag"
    "codeblockDual"
    "codeblockClose"
    "latexOpen"
    "latexClose"
    "latexDual"))

(defn token-from-text [^string text idx]
  (case (nth text 0)
    ")" (->Round idx (count text) :close)
    "(" (->Round idx (count text) :open)
    "]" (->Square idx (count text) :close)
    "[" (->Square idx (count text) :open)
    "{" (->Curly idx (count text) :open)
    "}" (->Curly idx (count text) :close)
    ":" (->Attribute idx (count text))
    "`" (->Backtick idx (count text))
    "!" (->Bang idx)
    "#" (->Hash idx)
    "-" (->Hr idx)
    nil))

(def escape-seq-regex (str "\\\\(?:" (utils/re-to-str rules/escapable-char-regex) ")"))


(defn to-regex [^string str] (js/RegExp. str "gm"))
;; TODO, faster to use interpose?
(def inline-re (to-regex (reduce #(str %1 "|" %2) escape-seq-regex
                                 (utils/eager-map #(utils/re-to-str (:regex %))
                                      regex-pieces))))
