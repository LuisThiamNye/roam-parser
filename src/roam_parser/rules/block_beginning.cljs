(ns roam-parser.rules.block-beginning
  (:require
   [roam-parser.transformations :as transf]
   [roam-parser.rules.relationships :refer [killed-by-of allowed-ctxs-fn]]
   [roam-parser.elements :as elements]
   [roam-parser.state :refer [lookahead-contains? update-last-ctx get-sub]]
   [roam-parser.utils :as utils]))

(defn form-hiccup [state] (let [hiccup-ctx (-> state :roam-parser.state/path peek)]
                            (assoc state :roam-parser.state/path
                                   (-> (:roam-parser.state/path state)
                                       pop
                                       (utils/update-last
                                        #(-> %
                                             (update :context/elements
                                                     conj
                                                     (elements/->Hiccup
                                                      (get-sub (:string state)
                                                               (:context/open-idx hiccup-ctx)
                                                               (:idx state))))
                                             (assoc :context/last-idx (:idx state))))))))

(defn start-hiccup [state char]
  (when (and  (identical? \: char)
              (lookahead-contains? state "hiccup"))
    (fn [state _]
      (-> state
          (update :roam-parser.state/path conj (-> {:context/id :context.id/hiccup
                                                    :context/open-idx 7
                                                    :context/start-idx (:idx state)
                                                    :context/rules (-> state :roam-parser.state/path peek :context/rules)
                                                    :context/elements []
                                                    :context/allows-ctx? (constantly false)
                                                    :context/killed-by #{}
                                                    :terminate-fallback form-hiccup}))
          (update :idx + 7)))))

(defn form-blockquote [state]
  (let [ctx (-> state :roam-parser.state/path peek)]
    (assoc state :roam-parser.state/path (-> (:roam-parser.state/path state)
                                             pop
                                             (transf/add-element (elements/->BlockQuote (:link-type ctx)
                                                                                        (transf/conj-text-el (:context/elements ctx)
                                                                                                             (:string state)
                                                                                                             ctx
                                                                                                             (:idx state)))
                                                                 state
                                                                 (:context/start-idx ctx)
                                                                 (:idx state))))))

(defn start-blockquote [state char]
  (when-some [[link-type length] (cond (and  (identical? \> char)
                                             (lookahead-contains? state \space))
                                       [:link-type/none 2]
                                       (and (identical? "[" char)
                                            (lookahead-contains? state "[>]] "))
                                       [:link-type/page 6]
                                       (and (identical? \# char)
                                            (lookahead-contains? state "[[>]] "))
                                       [:link-type/tag 7])]
    (fn [state get-fallbacks]
      (let [parent (-> state :roam-parser.state/path peek)]
        (-> state
            (update :roam-parser.state/path conj (-> {:context/id :context.id/block-quote
                                                      :link-type link-type
                                                      :context/open-idx length
                                                      :context/start-idx (:idx state)
                                                      :context/rules (:context/rules parent)
                                                      :context/elements []
                                                      :context/killed-by (killed-by-of :context.id/block-quote)
                                                      :context/allows-ctx? (allowed-ctxs-fn :context.id/block-quote (:context/allows-ctx? parent))
                                                      :terminate-fallback form-blockquote}
                                                     (transf/set-ctx-fallback state (get-fallbacks))))
            (update :idx + length))))))

(defn start-hr [state _]
  (when (identical? "---" (:string state))
    (transf/new-single-element (elements/->Hr) 3)))

(def rules [start-hiccup start-hr start-blockquote])

;; must be at the end of rules vector
(defn block-beginning-rules [state _]
  (fn [_ _]
    (transf/process-char-partially state rules (fn [state]
                                                 (update-last-ctx state #(update % :context/rules pop))))))
(comment
  (simple-benchmark [] ((constantly false) :bob) 10000)
  ;; 6
  (simple-benchmark [] (#(fn [x] false) :bob) 10000)
  ;; 5
  ;;;;;;;;;;;;;;
  )
