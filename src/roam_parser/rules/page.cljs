(ns roam-parser.rules.page
  (:require
   [roam-parser.rules.relationships :refer [allowed-ctxs killed-by-of]]
   [roam-parser.rules.text-bracket :refer [start-text-bracket-fn]]
   [roam-parser.state :refer [lookahead-contains?]]))

(defn terminate-page-link [state char]
  (when (identical? "]" char)
    (when (lookahead-contains? state "]")
      (let [idx     (:idx state)]
        (transf/ctx-to-element (:path state)
                               (fn [ctx]
                                 (elements/->PageLink
                                  (subs (:string state) (:context/open-idx ctx) idx)
                                  (:context/elements ctx)))
                               {:context/id :context.id/page-link
                                :killed-by (killed-by-of :context.id/page-link) ;; TODO use polymorphism? implement killed-by?
                                :next-idx   (+ 2 idx)})))))

(defn start-page-link [state char]
  (when (identical? "[" char)
    (let [double? (lookahead-contains? state "[")]
      (when double?
        (transf/try-new-ctx {:context/id        :context.id/page-link
                             :context/open-idx  (-> state :idx (+ 2))
                             :context/elements  []
                             :context/text-rules [(start-text-bracket-fn "[" "]")]
                             :context/killed-by (killed-by-of :context.id/page-link)
                             :context/allowed-ctxs (allowed-ctxs :context.id/page-link)
                             :context/terminate terminate-page-link}
                            state)))))
