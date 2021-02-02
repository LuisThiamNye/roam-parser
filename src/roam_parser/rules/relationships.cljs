(ns roam-parser.rules.relationships)

(defn killed-by-of [ctx-id]
  (case ctx-id
    :context.id/page-link       #{:context.id/image-square
                            :context.id/image-round}
    :context.id/bracket-tag     #{:context.id/image-square
                              :context.id/image-round}
    :context.id/alias-square    #{}
    :context.id/alias-round     #{}
    :context.id/image-square    #{}
    :context.id/image-round     #{}
    :context.id/parenthetical   #{}
    :context.id/block-ref       #{}
    :context.id/render          #{}
    :context.id/render-id       #{:context.id/render-id}
    :context.id/attribute       #{:context.id/codeblock}
    (:context.id/bold
     :context.id/highlight
     :context.id/italic
     :context.id/strikethrough) #{:context.id/codeblock
                                  :context.id/image-square
                                  :context.id/render}
    :context.id/codeblock       #{}
    :context.id/code            #{:context.id/codeblock}
    :context.id/block-quote     #{}
    :context.id/latex           #{}))

(def block-ctxs #{:context.id/code
                  :context.id/codeblock
                  :context.id/attribute
                  :context.id/render-id
                  :context.id/url
                  :context.id/latex
                  :context.id/bold
                  :context.id/highlight
                  :context.id/italic
                  :context.id/strikethrough
                  :context.id/alias-square
                  :context.id/image-square
                  :context.id/parenthetical
                  :context.id/page-link
                  :context.id/bracket-tag
                  :context.id/tag
                  :context.id/block-ref
                  :context.id/hr})

(defn allowed-ctxs [ctx-id]
  (case ctx-id
    :context.id/block         block-ctxs
    :context.id/render-id     #{:context.id/page-link}
    :context.id/page-link     #{:context.id/page-link
                                :context.id/bold
                                :context.id/highlight
                                :context.id/italic
                                :context.id/strikethrough
                                :context.id/alias-square}
    :context.id/bracket-tag   #{:context.id/page-link
                                :context.id/bold
                                :context.id/highlight
                                :context.id/italic
                                :context.id/strikethrough
                                :context.id/alias-square}
    :context.id/alias-square  #{:context.id/latex
                                :context.id/bold
                                :context.id/highlight
                                :context.id/italic
                                :context.id/strikethrough
                                :context.id/code
                                :context.id/image-square}
    :context.id/alias-round   #{:context.id/block-ref
                                :context.id/page-link}
    :context.id/image-square  #{:context.id/bold
                                :context.id/highlight
                                :context.id/italic
                                :context.id/strikethrough
                                :context.id/image-square}
    :context.id/image-round   #{}
    :context.id/parenthetical #{:context.id/code
                                :context.id/render
                                :context.id/url
                                :context.id/bold
                                :context.id/highlight
                                :context.id/italic
                                :context.id/strikethrough
                                :context.id/latex
                                :context.id/alias-square
                                :context.id/image-square
                                :context.id/parenthetical
                                :context.id/page-link
                                :context.id/bracket-tag
                                :context.id/tag
                                :context.id/block-ref}
    :context.id/block-ref     #{}
    :context.id/attribute     #{:context.id/codeblock}
    :context.id/bold          #{:context.id/codeblock
                                :context.id/image-square
                                :context.id/render
                                :context.id/highlight
                                :context.id/italic
                                :context.id/strikethrough}
    :context.id/highlight     #{:context.id/codeblock
                                :context.id/image-square
                                :context.id/render
                                :context.id/bold
                                :context.id/italic
                                :context.id/strikethrough}
    :context.id/italic #{:context.id/codeblock
                         :context.id/image-square
                         :context.id/render
                         :context.id/bold
                         :context.id/highlight
                         :context.id/strikethrough}
    :context.id/strikethrough #{:context.id/codeblock
                                :context.id/image-square
                                :context.id/render
                                :context.id/bold
                                :context.id/highlight
                                :context.id/italic}
    :context.id/codeblock     #{}
    :context.id/code          #{:context.id/codeblock}
    :context.id/block-quote   #{:context.id/code
                                :context.id/render
                                :context.id/url
                                :context.id/bold
                                :context.id/highlight
                                :context.id/italic
                                :context.id/strikethrough
                                :context.id/latex
                                :context.id/alias-square
                                :context.id/image-square
                                :context.id/parenthetical
                                :context.id/page-link
                                :context.id/bracket-tag
                                :context.id/tag
                                :context.id/block-ref
                                :context.id/hr}
    :context.id/latex         #{}))
