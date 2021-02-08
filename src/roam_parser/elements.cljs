(ns roam-parser.elements (:require [clojure.string]))

(defprotocol ElementProtocol
  "Operations on objects representing whole Roamdown elements within a block"
  (stringify [_]))

(defn text? [el] (string? el))


(defn stringify-children [els]
  (reduce #(str % (stringify %2)) "" els))


(declare Alias)
(declare Formatting)
(declare Image)
(declare Render)
(declare Curly)
(declare Round)
(declare Square)

(defrecord Hiccup [content]
  ElementProtocol
  (stringify [_]
    (str ":hiccup" content)))



(defrecord Codeblock [^string language ^string raw-content]
  ElementProtocol
  (stringify [_]
    (str "```" language \newline raw-content "\n```")))

(defrecord Code [raw-content]
  ElementProtocol
  (stringify [_]
    (str \` raw-content \`)))

(defrecord Latex [content]
  ElementProtocol
  (stringify [_]
    (str "$$" content "$$")))

(defrecord Url [url]
  ElementProtocol
  (stringify [_] url))

(defrecord Hr []
  ElementProtocol
  (stringify [_] "---"))

(defprotocol PageLinkProtocol
  (page-name [_]))

(defrecord PageLink
           [^string page-name namespaces-children]
  ElementProtocol
  (stringify [_]
    (str "[[" page-name "]]")))

(defrecord BracketTag [^string page-name namespaces-children]
  ElementProtocol
  (stringify [_]
    (str "#[[" page-name "]]")))

(defrecord Tag [^string page-name]
  ElementProtocol
  (stringify [_] (str \# page-name)))

(defrecord Alias [children destination-type destination]
  ElementProtocol
  (stringify [_]
    (str "[" (stringify-children children) "]("
         (case destination-type
           :block-ref (str "((" destination "))")
           :page (str "[[" destination "]]")
           :url destination)
         ")")))

(defrecord Image [children destination-type destination]
  ElementProtocol
  (stringify [_]
    (str "![" (stringify-children children) "]("
         (case destination-type
           :block-ref (str "((" destination "))")
           destination)
         ")")))

(defrecord AliasDestinationVirtual [children]
  ElementProtocol
  (stringify [_]
    (str "(" (stringify-children children) ")")))



(defrecord Parenthetical [children allow-block-ref?]
  ElementProtocol
  (stringify [_]
    (str "((" (stringify-children children) "))")))

(defrecord BlockRef [block-uid]
  ElementProtocol
  (stringify [_]
    (str "((" block-uid "))")))

(defrecord Formatting [format-type children]
  ElementProtocol
  (stringify [_]
    (let [delimiter (case format-type
                      :format-type/bold "**"
                      :format-type/italic "__"
                      :format-type/highlight "^^"
                      :format-type/strikethrough "~~")]
      (str delimiter (stringify-children children) delimiter))))

(defrecord Attribute [brackets? ^string page-name children]
  ElementProtocol
(stringify [_]
    (if brackets?
      (str "[[" (stringify-children children) "]]::")
      (str (stringify-children children) "::"))))

(defrecord Text [content]
  ElementProtocol
  (stringify [_] content))

(extend-type string
  ElementProtocol
  (stringify [s] s))

(defrecord Square [children]
  ElementProtocol
  (stringify [_] (str "[" (stringify-children children) "]")))

(defrecord Round [children]
  ElementProtocol
  (stringify [_] (str "(" (stringify-children children) ")")))

(defrecord Curly [children]
  ElementProtocol
  (stringify [_] (str "{" (stringify-children children) "}")))

(defrecord Render [id linked? children component]
  ElementProtocol
  (stringify [_]
    (str "{{" (when linked? "[[") id (when linked? "]]")
         (when (pos? (count children))
           (str ":" (stringify-children children)))
         "}}")))

(defrecord BlockQuote [link-type children]
  ElementProtocol
  (stringify [_]
    (str (case link-type :link-type/page "[[>]] " :link-type/tag "#[[>]] " "> ") (stringify-children children))))

(defrecord Block [children]
  ElementProtocol
  (stringify [_]
    (stringify-children children)))

(extend-type PersistentVector
  ElementProtocol
  (stringify [v] (stringify-children v)))
