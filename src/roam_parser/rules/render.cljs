(ns roam-parser.rules.render
  (:require
   [clojure.string]
   [roam-parser.rules.relationships :refer [allowed-ctxs killed-by-of]]
   [roam-parser.rules.text-bracket :refer [start-text-bracket-fn]]
   [roam-parser.state :refer [lookahead-contains? get-sub]]
   [roam-parser.elements :as elements]
   [roam-parser.transformations :as transf]))

(defn terminate-render [state char]
  (when (identical? "}" char)
    (when (lookahead-contains? state "}")
      (let [idx     (:idx state)]
        (transf/ctx-to-element (:path state)
                               (fn [ctx]
                                 (elements/->Render
                                  (:render/id ctx)
                                  (:linked? ctx)
                                  (:context/elements ctx)))
                               {:context/id :context.id/render
                                :killed-by (killed-by-of :context.id/render)
                                :next-idx   (+ 2 idx)})))))

(defn render-id-data [state]
  (let [ctx (-> state :path peek)
        els (:context/elements ctx)
        last-el (peek els)]
    (case (count els)
      0 (let [render-id (get-sub (:string state) (:context/open-idx ctx) (:idx state))]
          (when-not (clojure.string/blank? render-id)
            [false render-id]))
      1 (when (-> ctx :context/last-idx (identical? (:idx state)))
          [true (:page-name last-el)])
      nil)))

(defn terminate-render-id [state char]
  (cond (identical? char \:)
        (when-some [[linked? render-id] (render-id-data state)]
          (transf/swap-ctx (:path state)
                           {:context/id :context.id/render
                            :render/id render-id
                            :linked? linked?
                            :context/open-idx  (-> state :idx inc)
                            :context/elements  []
                            :context/text-rules [(start-text-bracket-fn  "{" "}")]
                            :context/killed-by (killed-by-of :context.id/render)
                            :context/allowed-ctxs #{:context.id/block-ref} ;; TODO adjust for id type
                            :context/terminate terminate-render}
                           {:context/id :context.id/render-id
                            :killed-by (killed-by-of :context.id/render-id)}))

        (and (identical? char "}")
             (lookahead-contains? state "}"))
        (when-some [[linked? render-id] (render-id-data state)]
          (transf/ctx-to-element (:path state)
                                 (fn [_]
                                   (elements/->Render render-id linked? nil))
                                 {:context/id :context.id/render-id
                                  :killed-by (killed-by-of :context.id/render-id)
                                  :next-idx   (-> state :idx (+ 2))}))))

(defn start-render [state char]
  (when (identical? "{" char)
    (let [double? (lookahead-contains? state "{")]
      (when double?
        (transf/try-new-ctx {:context/id        :context.id/render-id
                             :context/open-idx (-> state :idx (+ 2))
                             :context/elements  []
                             :context/killed-by (killed-by-of :context.id/render-id)
                             :context/allowed-ctxs (allowed-ctxs :context.id/render-id)
                             :context/terminate terminate-render-id}
                            state)))))
