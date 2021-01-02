(ns roam-parser.elements)

(defprotocol ElementProtocol
  "Operations on objects representing whole markdown elements within a block"
  (stringify [this] "Conjoins the raw string components of the element to the provided coll")
  (allowed-children [_])
  (killed-by [_]))

(defn- stringify-children [els]
  (reduce #(str % (stringify %2)) "" els))

(defrecord Block [children]
  ElementProtocol
  (allowed-children [_] #{:blockquote :hr :codeblock :code :render :page :bracket-tag
                          :alias :image :parenthetical :latex :bold :italic :highlight
                          :url :attribute :bracket-attribute :tag})
  (killed-by [_])
  (stringify [_]
    (stringify-children children)))

(defrecord Hiccup [content]
  ElementProtocol
  (allowed-children [_])
  (killed-by [_])
  (stringify [_]
    (str ":hiccup " content)))

(defrecord BlockQuote [link-type children]
  ElementProtocol
  (allowed-children [_] #{:page :render :image :bold :highlight :italic
                          :alias :parenthetical :hr :code :url :latex
                          :bracket-tag :tag :block-ref})
  (killed-by [_])
  (stringify [_]
    (str (case link-type :page "[[>]] " :tag "#[[>]] " "> ") (stringify-children children))))

(defrecord Codeblock [^string language ^string raw-content ^string content]
  ElementProtocol
  (allowed-children [_])
  (killed-by [_])
  (stringify [_]
    (str "```" language \newline raw-content "\n```")))

(defrecord Code [raw-content content]
  ElementProtocol
  (allowed-children [_])
  (killed-by [_] #{:codeblock})
  (stringify [_]
    (str \` raw-content \`)))

(defrecord Render []
  ElementProtocol
  (allowed-children [_] #{:page :block-ref :curly})
  (killed-by [_] #{:codeblock
                   :code})
  (stringify [_]
    ;;TODO
    ))

(defrecord PageLink
           [link-type ^string page-ns ^string page-name children]
  ElementProtocol
  (allowed-children [_] (case link-type
                          :page #{:page :bold :highlight :italic :alias}
                          :bracket-tag #{:page :bold :highlight :italic}
                          :tag nil))
  (killed-by [_] (case link-type
                   :page #{:image}
                   :bracket-tag  #{:image :alias}
                   :tag nil))
  (stringify [_]
    (str (case link-type :page "[[" :bracket-tag "#[[" :tag \#)
         (when (some? page-ns)  page-ns "/")
         (conj page-name)
         (when (not= :tag link-type) "]]"))))

(defrecord Alias [children destination-type destination]
  ElementProtocol
  (allowed-children [_]  #{:latex :code :image
                           :bold :highlight :italic})
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
  (allowed-children [_] #{:bold :highlight :italic
                          :image})
  (killed-by [_])
  (stringify [_]
    (str "[" (stringify-children children) "]("
         (case destination-type
           :block-ref (str "((" destination "))")
           destination)
         ")")))

(defrecord AliasDestinationVirtual [children]
  ElementProtocol
  (allowed-children [_] #{:page :block-ref})
  (killed-by [_] #{:alias})
  (stringify [_]
    (str "(" (stringify-children children) ")")))

(defrecord Parenthetical [children]
  ElementProtocol
  (allowed-children [_] #{:code :render :url :latex
                          :bold :highlight :italic
                          :page :alias :parenthetical :image
                          :bracket-tag :tag :block-ref})
  (killed-by [_] #{:codeblock})
  (stringify [_]
    (str "((" (stringify-children children) "))")))

(defrecord Latex [content]
  ElementProtocol
  (allowed-children [_])
  (killed-by [_] #{:codeblock})
  (stringify [_]
    (str "$$" content "$$")))

(defrecord Formatting [format-type children]
  ElementProtocol
  (allowed-children [_] #{:parenthetical :code :page :alias
                          :bold :italic :highlight :url})
  (killed-by [_] #{:image :render :latex})
  (stringify [_]
    (let [delimiter (case format-type
                      :bold "**"
                      :italic "__"
                      :highlight "^^")]
      (str delimiter (stringify-children children) delimiter))))

(defrecord Url [url]
  ElementProtocol
  (allowed-children [_])
  (killed-by [_])
  (stringify [_] url))

(defrecord Attribute [brackets? children]
  ElementProtocol
  (allowed-children [_] (if brackets?
                          #{:bold :italic :highlight :page}
                          #{:bold :italic :highlight}))
  (killed-by [_] (if brackets?
                    #{:image :alias}
                    #{:url :latex :block-ref :parenthetical
                                      :image :alias :bracket-tag :render :code}))
  (stringify [_]
    (if brackets?
      (str "[[" (stringify-children children) "]]::")
      (str (stringify-children children) "::"))))

(defrecord Hr []
  ElementProtocol
  (allowed-children [_])
  (killed-by [_])
  (stringify [_] "---"))

(defrecord Text [content]
  ElementProtocol
  (allowed-children [_])
  (killed-by [_])
  (stringify [_] content))

(defrecord Curly [children]
  ElementProtocol
  (allowed-children [_] #{:page :block-ref :curly})
  (killed-by [_] #{:codeblock
                   :code})
  (stringify [_] (str "{" (stringify-children children) "}")))
