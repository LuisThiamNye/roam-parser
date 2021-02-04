(ns roam-parser.rules.alias
  (:require
   [roam-parser.rules.relationships :refer [killed-by-of]]
   [roam-parser.elements :as elements]
   [roam-parser.transformations :as transf]
   [roam-parser.rules.text-bracket :refer [start-text-bracket-fn]]
   [roam-parser.state :refer [lookahead-contains? string-contents]]))

(defn terminate-alias-round-fn [round-id]
  (fn [state char]
    (when (identical? ")" char)
      (transf/ctx-to-element (:path state)
                             (case round-id
                               :context.id/alias-round
                               (fn [ctx]
                                 (let [dest-els    (:context/elements ctx)
                                       first-child (first dest-els)
                                       [dest-type
                                        dest]      (or (cond (instance? elements/BlockRef first-child)
                                                             [:block-ref (:block-uid first-child)]

                                                             (instance? elements/PageLink first-child)
                                                             [:page (:page-name first-child)])

                                                       [:url (string-contents ctx state)])
                                       square-els (:context/elements (:context/replaced-ctx ctx))]
                                   (when (pos? (count square-els))
                                     (elements/->Alias square-els dest-type dest))))

                               :context.id/image-round
                               (fn [ctx]
                                 (let [dest (string-contents ctx state)]
                                   (when (pos? (count dest))
                                     (elements/->Image (-> ctx :context/replaced-ctx :context/elements)
                                                       :url
                                                       dest)))))
                             {:context/id round-id
                              :killed-by  (killed-by-of round-id)
                              :next-idx   (-> state :idx inc)}))))

(defn terminate-alias-square-fn [square-id round-id]
  (fn [state char]
    (when (identical? "]" char)
      (if  (lookahead-contains? state "(")
        (transf/swap-ctx (:path state)
                         {:context/id round-id
                          :context/open-idx  (-> state :idx (+ 2))
                          :context/elements  []
                          :context/text-rules [(start-text-bracket-fn  "(" ")")]
                          :context/killed-by (killed-by-of round-id)
                          :context/terminate (terminate-alias-round-fn round-id)}
                         {:context/id square-id
                          :killed-by (killed-by-of square-id)})
        (fn [state _]
          (transf/fallback-from-open (-> state :path peek)))))))

(defn start-alias-square [state char]
  (when (identical? "[" char)
    (transf/try-new-ctx {:context/id        :context.id/alias-square
                         :context/open-idx  (-> state :idx inc)
                         :context/elements  []
                         :context/text-rules [(start-text-bracket-fn "[" "]")]
                         :context/killed-by (killed-by-of :context.id/alias-square)
                         :context/terminate (terminate-alias-square-fn :context.id/alias-square :context.id/alias-round)} state)))

(defn start-image-square [state char]
  (when (and (identical? \! char)
             (lookahead-contains? state "["))
    (transf/try-new-ctx {:context/id :context.id/image-square
                         :context/open-idx (-> state :idx (+ 2))
                         :context/elements []
                         :context/text-rules [(start-text-bracket-fn "[" "]")]
                         :context/killed-by (killed-by-of :context.id/image-square)
                         :context/terminate (terminate-alias-square-fn :context.id/image-square :context.id/image-round)}
                        state)))
