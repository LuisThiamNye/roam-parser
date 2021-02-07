(ns roam-parser.elements (:require [clojure.string]))

(defprotocol ElementProtocol
  "Operations on objects representing whole markdown elements within a block"
  (stringify [this] "Conjoins the raw string components of the element to the provided coll")
  (allowed-children [_])
  (killed-by [_]))

(defn text? [el] (string? el))


(defn- stringify-children [els]
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
  (allowed-children [_])
  (killed-by [_])
  (stringify [_]
    (str ":hiccup" content)))



(defrecord Codeblock [^string language ^string raw-content]
  ElementProtocol
  (allowed-children [_])
  (killed-by [_])
  (stringify [_]
    (str "```" language \newline raw-content "\n```")))

(defrecord Code [raw-content]
  ElementProtocol
  (allowed-children [_])
  (killed-by [_] #{Codeblock})
  (stringify [_]
    (str \` raw-content \`)))

(defrecord Latex [content]
  ElementProtocol
  (allowed-children [_])
  (killed-by [_] #{Codeblock})
  (stringify [_]
    (str "$$" content "$$")))

(defrecord Url [url]
  ElementProtocol
  (allowed-children [_])
  (killed-by [_])
  (stringify [_] url))

(defrecord Hr []
  ElementProtocol
  (allowed-children [_])
  (killed-by [_])
  (stringify [_] "---"))

(defprotocol PageLinkProtocol
  (page-name [_]))

(defrecord PageLink
           [^string page-name namespaces-children]
  ElementProtocol
  (allowed-children [_] #{PageLink  Formatting Alias})
  (killed-by [_]  #{Image})
  (stringify [_]
    (str "[[" page-name "]]")))

(defrecord BracketTag [^string page-name namespaces-children]
  ElementProtocol
  (allowed-children [_]  #{PageLink  Formatting})
  (killed-by [_]  #{Image Alias})
  (stringify [_]
    (str "#[[" page-name "]]")))

(defrecord Tag [^string page-name ns-strings]
  ElementProtocol
  (stringify [_] (str \# page-name)))

(defrecord Alias [children destination-type destination]
  ElementProtocol
  (allowed-children [_]  #{Latex Code Image
                           Formatting})
  (killed-by [_])
  (stringify [_]
    (str "[" (stringify-children children) "]("
         (case destination-type
           :block-ref (str "((" destination "))")
           :page (str "[[" destination "]]")
           :url destination)
         ")")))

(defrecord Image [children destination-type destination]
  ElementProtocol
  (allowed-children [_] #{Formatting
                          Image})
  (killed-by [_])
  (stringify [_]
    (str "![" (stringify-children children) "]("
         (case destination-type
           :block-ref (str "((" destination "))")
           destination)
         ")")))

(defrecord AliasDestinationVirtual [children]
  ElementProtocol
  (allowed-children [_] #{PageLink})
  (killed-by [_] #{Alias})
  (stringify [_]
    (str "(" (stringify-children children) ")")))



(defrecord Parenthetical [children allow-block-ref?]
  ElementProtocol
  (allowed-children [_] #{Code Render Url Latex
                          Formatting
                          PageLink Alias Parenthetical Image
                          BracketTag Tag})
  (killed-by [_] #{Codeblock})
  (stringify [_]
    (str "((" (stringify-children children) "))")))

(defrecord BlockRef [block-uid]
  ElementProtocol
  (allowed-children [_] #{})
  (killed-by [_] #{Codeblock})
  (stringify [_]
    (str "((" block-uid "))")))

(defrecord Formatting [format-type children]
  ElementProtocol
  (allowed-children [_] #{Parenthetical Code PageLink Alias
                          Formatting Url})
  (killed-by [_] #{Image Render Latex})
  (stringify [_]
    (let [delimiter (case format-type
                      :format-type/bold "**"
                      :format-type/italic "__"
                      :format-type/highlight "^^"
                      :format-type/strikethrough "~~")]
      (str delimiter (stringify-children children) delimiter))))

(defrecord Attribute [brackets? ^string page-name children]
  ElementProtocol
  (allowed-children [_] (if brackets?
                          #{Formatting PageLink}
                          #{Formatting}))
  (killed-by [_] (if brackets?
                   #{Image Alias}
                   #{Url Latex Parenthetical
                     Image Alias BracketTag Render Code}))
  (stringify [_]
    (if brackets?
      (str "[[" (stringify-children children) "]]::")
      (str (stringify-children children) "::"))))

(defrecord Text [content]
  ElementProtocol
  (allowed-children [_])
  (killed-by [_])
  (stringify [_] content))

(extend-type string
  ElementProtocol
  (allowed-children [_])
  (killed-by [_])
  (stringify [s] s))

(defrecord Square [children]
  ElementProtocol
  (allowed-children [_] #{PageLink Parenthetical Curly Round Square})
  (killed-by [_] #{Codeblock
                   Code})
  (stringify [_] (str "[" (stringify-children children) "]")))

(defrecord Round [children]
  ElementProtocol
  (allowed-children [_] #{PageLink Parenthetical Curly Round Square})
  (killed-by [_] #{Codeblock
                   Code})
  (stringify [_] (str "(" (stringify-children children) ")")))

(defrecord Curly [children]
  ElementProtocol
  (allowed-children [_] #{PageLink Parenthetical Curly Round Square})
  (killed-by [_] #{Codeblock
                   Code})
  (stringify [_] (str "{" (stringify-children children) "}")))

(defrecord Render [id linked? children component]
  ElementProtocol
  (allowed-children [_] #{PageLink Parenthetical Curly Round Square})
  (killed-by [_] #{Codeblock
                   Code})
  (stringify [_]
    (str "{{" (when linked? "[[") id (when linked? "]]")
         (when (pos? (count children))
           (str ":" (stringify-children children)))
         "}}")))

(defrecord BlockQuote [link-type children]
  ElementProtocol
  (allowed-children [_] #{PageLink Render Image  Formatting
                          Alias Parenthetical Hr Code Url Latex
                          BracketTag Tag})
  (killed-by [_])
  (stringify [_]
    (str (case link-type :link-type/page "[[>]] " :link-type/tag "#[[>]] " "> ") (stringify-children children))))

(defrecord Block [children]
  ElementProtocol
  (allowed-children [_] #{BlockQuote Hr Codeblock Code Render PageLink BracketTag
                          Alias Image Parenthetical Latex Formatting
                          Url Attribute Tag})
  (killed-by [_])
  (stringify [_]
    (stringify-children children)))

(extend-type PersistentVector
  ElementProtocol
  (stringify [v] (stringify-children v)))
