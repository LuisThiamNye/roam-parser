(ns roam-parser.rules.render.sub-ctxs
  (:require
   [clojure.string]
   [roam-parser.rules.text-bracket :refer [start-text-bracket-fn]]
   [roam-parser.elements :as elements]
   [roam-parser.transformations :as transf]
   [roam-parser.state :as state]))

(def form-white-space-re #"[\s,]+")

(defn into-forms [els]
  (reduce (fn [els-acc el]
            (if (string? el)
              (-> (clojure.string/split el form-white-space-re)
                  (as-> splits (cond-> splits
                                 (-> splits first (identical? ""))
                                 rest))
                  (->> (into els-acc)))
              (conj els-acc el)))
          [] els))

(defrecord CurlyList [children]
  elements/ElementProtocol
  (stringify [_] (str "{" (elements/stringify-children children) "}")))

(defrecord StringElement [content]
  elements/ElementProtocol
  (stringify [_] (str \" content \")))

(defn terminate-string [state char]
  (when (identical? \" char)
    (transf/ctx-to-element (::state/path state)
                           (fn [ctx]
                             (->StringElement (-> ctx :context/elements peek)))
                           {:context/id :context.id/string
                            :killed-by #{}
                            :next-idx (-> state :idx inc)})))

(defn start-string [state char]
  (when (identical? \" char)
    (transf/try-new-ctx {:context/id        :context.id/string
                         :context/open-idx  (-> state :idx inc)
                         :context/elements  []
                         :context/allows-ctx? (constantly false)
                         :context/killed-by #{}
                         :context/terminate terminate-string}
                        state)))

(defn terminate-curly-list [state char]
  (when (identical? "}" char)
    (transf/ctx-to-element (::state/path state)
                           (fn [ctx]
                             (->CurlyList (into-forms (:context/elements ctx))))
                           {:context/id :context.id/curly-list
                            :killed-by #{}
                            :next-idx (-> state :idx inc)})))

(defn start-curly-list [state char]
  (when (identical? "{" char)
    (let [parent (-> state ::state/path peek)]
      (transf/try-new-ctx {:context/id        :context.id/curly-list
                           :context/open-idx  (-> state :idx inc)
                           :context/elements  []
                           :context/text-rules [(start-text-bracket-fn "{" "}")]
                           :context/allows-ctx? #(or (:context/allows-ctx? parent)
                                                      (#{:context.id/string} %))
                           :context/killed-by #{}
                           :context/terminate terminate-curly-list}
                          state))))

(defn start-pipe [state char]
  (when (identical? \| char)
    (transf/new-single-element \| (:idx state) (-> state :idx inc))))
