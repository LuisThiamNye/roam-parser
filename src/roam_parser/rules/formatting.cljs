(ns roam-parser.rules.formatting
  (:require
   [clojure.string]
   [roam-parser.rules.relationships :refer [killed-by-of]]
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

(defn only-two-closers? [state char]
  (and (re-find (re-pattern (str "^\\" char "(?!\\" char ")"))
            (remaining-str state))
   (re-find (re-pattern (str "[^\\s\\" char  "]$"))
            (preceding-str state))))

(defn terminate-formatting-fn [ctx-id match-char]
  (fn [state char]
    (when (and (identical? char match-char)
               (only-two-closers? state match-char))
      (transf/ctx-to-element (:path state)
                             (fn [ctx]
                               (elements/->Formatting (ctx->format-id ctx-id)
                                                      (:context/elements ctx)))
                             {:context/id ctx-id
                              :killed-by (killed-by-of ctx-id)
                              :next-idx (-> state :idx (+ 2))}))))

(defn only-two-openers? [state char]
  (and (re-find (re-pattern  (str "^\\" char "[^\\s\\" char "]"))
                (remaining-str state))
       (re-find (re-pattern (str "(?<!\\" char ")$"))
                (preceding-str state))))

(defn start-formatting [state char]
  (when-let [ctx-id (char->ctx-id char)]
    (when (only-two-openers? state char)
      (transf/try-new-ctx {:context/id ctx-id
                           :context/open-idx (-> state :idx (+ 2))
                           :context/elements []
                           :context/killed-by (killed-by-of ctx-id)
                           :context/terminate (terminate-formatting-fn ctx-id char)}
                          state))))

(defn terminate-latex [state char]
  (when (and (identical? \$ char)
             (only-two-closers? state char))
    (transf/ctx-to-element (:path state)
                           (fn [ctx]
                             (let [content (-> ctx :context/elements peek)]
                               (when-not (clojure.string/blank? content)
                                 (elements/->Latex content))))
                           {:context/id :context.id/latex
                            :killed-by (killed-by-of :context.id/latex)
                            :next-idx (-> state :idx (+ 2))})))

(defn start-latex [state char]
  (when (and  (identical? char \$)
              (only-two-openers? state char))
    (transf/try-new-ctx {:context/id :context.id/latex
                         :context/open-idx (-> state :idx (+ 2))
                         :context/killed-by (killed-by-of :context.id/latex)
                         :context/terminate terminate-latex}
                        state)))
