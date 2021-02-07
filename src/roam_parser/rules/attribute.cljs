(ns roam-parser.rules.attribute
  (:require
   [clojure.string]
   [taoensso.timbre :as t]
   [roam-parser.utils :refer [no-blank-ends?]]
   [roam-parser.state :refer [lookahead-contains? update-last-ctx initial-state]]
   [roam-parser.elements :as elements]
   [roam-parser.transformations :as transf]
   [roam-parser.rules.relationships :refer [killed-by-of allowed-ctxs-fn]]))

(defn attr-delimiter? [state char]
  (and (identical? char \:)
       (lookahead-contains? state \:)))

(defn terminate-attribute [state char]
  (when (attr-delimiter? state char)
    (let [idx (:idx state)]
      (transf/ctx-to-element (:roam-parser.state/path state)
                             (fn [ctx]
                               (let [page-name (subs (:string state) (:context/open-idx ctx) idx)]
                                 (when (no-blank-ends? page-name)
                                   (elements/->Attribute
                                    ;; no brackets
                                    false
                                    page-name
                                    (:context/elements ctx)))))
                             {:context/id :context.id/attribute
                              :killed-by (killed-by-of :context.id/attribute)
                              :next-idx (+ 2 idx)}))))

(defn start-attribute [state char]
  (when (and (attr-delimiter? state char)
             (not (:has-attribute state))
             (-> state :roam-parser.state/path peek :context/id #{:context.id/block}))
    (let [parent (-> state :roam-parser.state/path peek)
          new-state (initial-state (:string state) (:context/rules parent))]
      (fn [state get-fallbacks]
        (let [ctx {:context/id        :context.id/attribute
                   :context/open-idx (:context/open-idx parent)
                   :context/elements  []
                   :context/killed-by (killed-by-of :context.id/attribute)
                   :context/terminate terminate-attribute}
              new-ctx (-> ctx
                          (assoc  :context/rules
                                  (conj (-> new-state :roam-parser.state/path peek :context/rules)
                                        (:context/terminate ctx)))
                          (cond-> (nil? (:context/allows-ctx? ctx))
                            (assoc :context/allows-ctx?
                                   (allowed-ctxs-fn (:context/id ctx) (:context/allows-ctx? parent))))
                          (assoc :context/start-idx (:idx new-state))
                          (transf/set-ctx-fallback state (get-fallbacks)))]
          (t/debug "START NEW CTX\n" new-ctx)
          (-> new-state
              (update :roam-parser.state/path conj new-ctx)
              (assoc :has-attribute true)
              (assoc :idx (:context/open-idx ctx))))))))
