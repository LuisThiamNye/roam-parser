(ns roam-parser.rules.formatting
  (:require
   [roam-parser.rules.relationships :refer [ killed-by-of]]
   [roam-parser.transformations :as transf]
   [roam-parser.elements :as elements]
   [roam-parser.state :refer [remaining-str preceding-str]]))

(defn ctx->format-id [ctx-id]
  (case ctx-id
    :context.id/italic :format-type/italic
    :context.id/highlight :format-type/highlight
    :context.id/bold :format-type/bold
    :context.id/strikethrough :format-type/strikethrough))

(defn char->ctx-id [char]
  (case char
    \* :context.id/bold
    \^ :context.id/highlight
    \_ :context.id/italic
    \~ :context.id/strikethrough
    nil))

(defn terminate-formatting-fn [ctx-id match-char]
  (fn [state char]
    (when (and (identical? char match-char)
               (re-find (re-pattern (str "^\\" match-char "(?!\\" match-char ")"))
                        (remaining-str state))
               (re-find (re-pattern (str "[^\\s\\" match-char  "]$"))
                        (preceding-str state)))
      (transf/ctx-to-element (:path state)
                             (fn [ctx]
                               (elements/->Formatting (ctx->format-id ctx-id)
                                                      (:context/elements ctx)))
                             {:context/id ctx-id
                              :killed-by (killed-by-of ctx-id)
                              :next-idx (-> state :idx (+ 2))}))))

(defn start-formatting [state char]
  (when-let [ctx-id (char->ctx-id char)]
    (when (and (re-find (re-pattern  (str "^\\" char "[^\\s\\" char "]"))
                        (remaining-str state))
               (re-find (re-pattern (str "(?<!\\" char ")$"))
                        (preceding-str state)))
      (transf/try-new-ctx {:context/id ctx-id
                           :context/open-idx (-> state :idx (+ 2))
                           :context/elements []
                           :context/killed-by (killed-by-of ctx-id)
                           :context/terminate (terminate-formatting-fn ctx-id char)}
                          state))))
