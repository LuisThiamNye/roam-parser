(ns roam-parser.rules (:require [clojure.set]))



(def escapable-char-regex #"[\\{}\[\]\(\)`\*_\^:#!\n>]|\${2}")

(def codeblock-re (js/RegExp #"^(?:(javascript|css|html|clojure|common lisp)\n|\n?)(.*(?:[^\n]|$))" "s"))
(def default-codeblock-lang "javascript")

(def style-el-definition (array-map :allowed-children #{:parenthetical :code :page :alias
                                                        :bold :italic :highlight :url}
                                    :killed-by #{:image :render :latex}
                                    :scope :inline))

(def -element-definitions {:block (array-map :allowed-children #{:page :render :image :bold :highlight :italic
                                                                :alias :parenthetical :hr :code :url :latex
                                                                :bracket-tag :tag :block-ref :codeblock :attribute})
                          :blockquote (array-map :allowed-children #{:page :render :image :bold :highlight :italic
                                                                     :alias :parenthetical :hr :code :url :latex
                                                                     :bracket-tag :tag :block-ref})
                          :codeblock (array-map)
                          :code (array-map :killed-by #{:codeblock})
                          :render (array-map :allowed-children #{:page :block-ref :curly}
                                             :killed-by #{:codeblock
                                                          :code}
                                             :delimiters #{:curly}
                                             :invisible? :true)
                          :curly (array-map :allowed-children #{:page :block-ref :curly}
                                            :killed-by #{:codeblock
                                                         :code})
                          :page (array-map :allowed-children #{:page :bold :highlight :italic :alias}
                                           :delimiters #{:square}
                                           :scope :inline
                                           :killed-by #{:image})
                          :bracket-tag (array-map :allowed-children #{:page :bold :highlight :italic}
                                                  :delimiters #{:square}
                                                  :scope :inline
                                                  :killed-by #{:image :alias})
                          :alias (array-map :allowed-children #{:latex :code :image
                                                                :bold :highlight :italic}
                                            :delimiters #{:square :round})
                          :image (array-map :allowed-children #{:bold :highlight :italic
                                                                :image}
                                            :delimiters #{:square :round})
                          ;; dummy element; never created
                          :alias-destination (array-map :allowed-children #{:page :block-ref}
                                                        :killed-by #{:alias}
                                                        :scope :inline
                                                        :invisible? :true)
                          :parenthetical (array-map :allowed-children #{:code :render :url :latex
                                                                        :bold :highlight :italic
                                                                        :page :alias :parenthetical :image
                                                                        :bracket-tag :tag :block-ref}
                                                    :killed-by #{:codeblock}
                                                    :delimiters #{:round})
                          :latex (array-map :killed-by #{:codeblock})
                          :bold style-el-definition :italic style-el-definition :highlight style-el-definition

                          ;; singles

                          :hr (array-map)
                          :url (array-map)
                          :attribute (array-map :allowed-children #{:bold :italic :highlight}
                                                :killed-by #{:url :latex :block-ref :parenthetical
                                                             :image :alias :bracket-tag :render :code})
                          :bracket-attribute (array-map :allowed-children #{:bold :italic :highlight :page}
                                                        :killed-by #{:image :alias})
                          ;; tags cannot overlap with other delimiters due to regex
                          :tag (array-map)})
