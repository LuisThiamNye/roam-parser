(ns roam-parser.rules.parenthetical
  (:require
   [roam-parser.utils :refer [no-blank-ends?]]
   [roam-parser.transformations :as transf]
   [roam-parser.state :refer [lookahead-contains?]]
   [roam-parser.rules.relationships :refer [killed-by-of]]
   [roam-parser.rules.text-bracket :refer [start-text-bracket-fn]]
   [roam-parser.elements :as elements]))

(defn terminate-parenthetical-fn [id]
  (fn [state char]
    (when (and (identical? char ")")
               (lookahead-contains? state ")"))
      (transf/ctx-to-element (:path state)
                             (case id
                               :context.id/parenthetical
                               (fn [ctx]
                                 (elements/->Parenthetical (:context/elements ctx)
                                                           (:parenthetical/allow-block-ref? ctx)))
                               :context.id/block-ref
                               (fn [ctx]
                                 (let [uid (-> ctx :context/elements peek)]
                                   (when (no-blank-ends? uid)
                                     (elements/->BlockRef uid)))))
                             {:context/id id
                              :killed-by (killed-by-of id) ;; TODO use polymorphism? implement killed-by?
                              :next-idx   (+ 2 (:idx state))}))))

(defn start-parenthetical [state char]
  (when (and (identical? char "(")
             (lookahead-contains? state "("))
    (let [allows-ctx? (:context/allows-ctx? (-> state :path peek))
          id (if (allows-ctx? :context.id/parenthetical)
               :context.id/parenthetical
               :context.id/block-ref)]
      (transf/try-new-ctx {:context/id id
                           :parenthetical/allow-block-ref? (allows-ctx? :context.id/block-ref)
                           :context/open-idx  (-> state :idx (+ 2))
                           :context/elements  []
                           :context/text-rules [(start-text-bracket-fn "(" ")")]
                           :context/killed-by (killed-by-of id)
                           :context/terminate (terminate-parenthetical-fn id)}
                          state))))
