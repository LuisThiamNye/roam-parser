(ns roam-parser.tokens.paired (:require [roam-parser.builder :as builder]
                                        [roam-parser.utils :as utils]
                                        [roam-parser.tokens.token :as tkn]
                                        [roam-parser.render :as render]
                                        [roam-parser.elements :as elements]))

(defn get-close-bracket-data
  [parser-parameters initial-state current-token partner]
  (let [{:keys [length idx]} current-token
        quantity (min (:length partner) length)
        start-idx (- (+ (:idx partner) (:length partner)) quantity)
        end-idx (+ idx quantity)
        inner-end (- end-idx quantity)
        inner-start (+ start-idx quantity)
        excess-closers? (and (> length quantity) (= :close (:direction current-token)))
        excess-openers? (> (:length partner) quantity)
        children-tokens (-> (:tokens parser-parameters)
                                        ;; split up excess closers
                            (builder/update-token (type current-token) (:idx current-token)
                                                  (fn [tkn] (if (not= (:idx current-token) (:idx tkn))
                                                              (update tkn :length - (:length current-token)) tkn)))

                             ;; split up excess openers
                            (builder/update-token (type current-token) (:idx partner)
                                                  #(if (not= (:length partner) (:length %))
                                                     (-> %
                                                         (update :length - (:length partner))
                                                         (update :idx + (:length partner)))
                                                     %)))
        add-bracket-el-with-children #(builder/add-element-with-children %1 (assoc %2
                                                                                   :children-start inner-start
                                                                                   :children-end inner-end)
                                                                         %3)]
    (hash-map :quantity quantity
              :start-idx start-idx
              :end-idx end-idx
              :inner-start inner-start
              :inner-end inner-end
              :children-tokens children-tokens
              :add-bracket-element-with-children #(add-bracket-el-with-children
                                                   % %2 (hash-map :parser-parameters parser-parameters
                                                                                               :children-tokens children-tokens))
              :not-empty (pos? (- inner-end inner-start))
              :current-state (as-> initial-state state
                               ;; add excess closers to the queue to be processed next
                               (if excess-closers?
                                 (update state :pending-series-tokens assoc 0 (-> current-token
                                                                                  (update :length - quantity)
                                                                                  (update :idx + quantity)))
                                 (update state :pending-series-tokens subvec 1))
                               ;; split opener into higher level, outer opener if long enough
                               (if excess-openers?
                                 (update state :free-openers assoc (-> state :free-openers count dec) (update partner :length - quantity))
                                 (update state :free-openers pop))))))

(defn valid-alias-opening-round? [round-tkn]
  (contains? #{1 3} (:length round-tkn)))

(defn close-square [parser-parameters initial-state {:keys [idx] :as current-token} partner]
  (let [{:keys [quantity end-idx start-idx add-bracket-element-with-children
                children-tokens
                not-empty current-state]} (get-close-bracket-data parser-parameters initial-state current-token partner)
        {:keys [pending-tokens]} current-state
        round-tks (get pending-tokens tkn/Round)
        [following-round-vec-idx following-round-tkn] (loop [i (dec (count round-tks))]
                                                        (if (neg? i)
                                                          [nil nil]
                                                          (let [tkn (nth round-tks i)]
                                                            (if (and (= end-idx (:idx tkn)) (= :open (:direction tkn)))
                                                              [i tkn]
                                                              (recur (dec i))))))
        could-be-page (> quantity 1)]
    (or (cond
          ;; case: this is potential [alias text]()
          (and (some? following-round-vec-idx)
               (valid-alias-opening-round? following-round-tkn))
          (let [alias-id (if (builder/find-token pending-tokens tkn/Bang (dec start-idx))
                           :image
                           :alias)]
            (when (or not-empty (not (= :alias alias-id)))
              (-> current-state
                  (update :potential-alias-squares #(let [data {:end end-idx
                                                                :start start-idx
                                                                :children-start (+ start-idx 1)
                                                                :children-end (- end-idx 1)
                                                                :deferred-data {:children-tokens children-tokens}}]
                                                      (if (nil? %)
                                                        (vector data)
                                                        (conj % data))))
                                  ;; mark opening round as potential alias


                  (assoc-in [:pending-tokens tkn/Round following-round-vec-idx :tag] alias-id))))

          ;; case: this IS a [[page]]
          could-be-page
          (let [brackets? (builder/find-token pending-tokens tkn/Hash (dec start-idx))
                base-data {:end end-idx
                           :start (cond-> start-idx brackets? dec)
                           :page-name (builder/get-sub (:block-string parser-parameters) (+ start-idx quantity) (- end-idx quantity))}]
            (when not-empty
              (add-bracket-element-with-children current-state
                                                 (if brackets?
                                                   (elements/map->BracketTag base-data)
                                                   (elements/map->PageLink base-data))))))
        current-state)))

(defn close-round [parser-parameters initial-state {:keys [idx] :as current-token} partner]
  (let [{:keys [quantity end-idx start-idx
                inner-start
                inner-end not-empty add-bracket-element-with-children
                current-state]} (get-close-bracket-data parser-parameters initial-state current-token partner)]
    (or (if (and (contains? #{:alias :image} (:tag partner)) (= quantity (:length partner)))

          ;; make an alias
          (let [squares (:potential-alias-squares current-state)
                square-pair (some #(when (identical? (:idx partner) (:end %)) %) squares)
                square-data (:deferred-data square-pair)
                element-id (:tag partner)
                [dest-type
                 dest] (cond
                         (and (identical? quantity 3) not-empty)
                         [:block-ref (builder/get-sub (:block-string parser-parameters) inner-start inner-end :no-escape)]

                         (and (identical? quantity 1)
                              (or not-empty (= :alias (:tag partner))))
                         (let [virtual-dest (elements/map->AliasDestinationVirtual
                                             {:children-start inner-start
                                              :children-end inner-end})
                               children (:children (builder/process-children
                                                    (assoc parser-parameters
                                                           :el-type-allowed? #(contains? (elements/allowed-children virtual-dest)
                                                                                         %)
                                                           :tokens (builder/find-children-tokens (:tokens parser-parameters) inner-start inner-end)
                                                           :parent virtual-dest)))
                               first-child (first children)]
                           (if (and (identical? 1 (count children))
                                    (instance? elements/PageLink first-child)
                                    (= inner-start (:start first-child))
                                    (= inner-end (:end first-child)))
                             [:page (:page-name first-child)]
                             [:url (builder/get-sub (:block-string parser-parameters) inner-start inner-end)])))]
            (when dest-type
              (builder/add-element-with-children current-state
                                                 (cond-> {:start (cond-> (:start square-pair)
                                                                   (= :image element-id) dec)
                                                          :divider-idx (dec (:end square-pair))
                                                          :end end-idx
                                                          :children-start (:children-start square-pair)
                                                          :children-end (:children-end square-pair)
                                                          :destination-type dest-type
                                                          :destination dest}
                                                   (= :image element-id) (elements/map->Image)
                                                   (= :alias element-id) (elements/map->Alias))
                                                 (hash-map :children-tokens (:children-tokens square-data)
                                                           :parser-parameters parser-parameters))))

          ;; (()) block ref or parenthetical
          (when not-empty
            (if (> quantity 1)
              (add-bracket-element-with-children current-state (-> {:start start-idx
                                                                    :end end-idx}
                                                                   (elements/map->Parenthetical)))

              ;;  a normal pair of ( ) brackets, discard and surface children as an isolated bunch
              ;; TODO only process url as children
              (-> current-state
                  (assoc :elements
                         (:children (builder/process-children
                                     (assoc parser-parameters
                                            :tokens {tkn/Url (utils/eager-filter (builder/children-token-pred (:idx partner) idx)
                                                                                 (get-in parser-parameters [:tokens tkn/Url]))}
                                            :parent (assoc (:parent parser-parameters) :children-end inner-end :children-start inner-start)
                                            :text-mode :no-text))))
                  (assoc :pending-tokens (builder/find-sibling-tokens (:pending-tokens current-state) start-idx idx tkn/Url))))))
        current-state)))

(defn close-curly [parser-parameters initial-state {:keys [idx] :as current-token} partner]
  (let [{:keys [quantity end-idx start-idx children-tokens
                add-bracket-element-with-children inner-start inner-end
                not-empty current-state]} (get-close-bracket-data parser-parameters initial-state current-token partner)
        {:keys [free-openers]} current-state]
    (if (and not-empty (>= quantity 2))
      (if-let [render-el (render/init-render-element
                       (:block-string parser-parameters)
                       {:start (- (+ (:idx partner) (:length partner)) 2)
                        :end (+ idx 2)
                        :children-start inner-start
                        :children-end inner-end})]
        (builder/add-element-with-children current-state
render-el
                                           {:override-allowed-rules true
                                            :parser-parameters parser-parameters
                                            :children-tokens children-tokens})
        current-state)
      (add-bracket-element-with-children current-state
                                         (elements/map->Curly {:start start-idx
                                                               :end end-idx})))))


(defn close-ambiguous-fn [min-delimiter-length add-extra no-children?]
  (fn [parser-parameters current-state current-token partner]
    (let [{:keys [idx]} current-token
          length (or (:length current-token min-delimiter-length))
          end-idx                 (+ idx length)
          start-idx               (:idx partner)
          inner-start             (+ min-delimiter-length start-idx)
          inner-end               (- end-idx min-delimiter-length)]
      (-> current-state
          (update :free-openers pop)
          (update :pending-series-tokens subvec 1)
          (builder/add-element-with-children (add-extra (hash-map :start start-idx
                                                                  :end   end-idx
                                                                  :children-start inner-start
                                                                  :children-end inner-end)
                                                        parser-parameters)
                                             (hash-map :parser-parameters parser-parameters
                                                       :children-tokens (:tokens parser-parameters)
                                                       :no-children? no-children?))))))

(defn on-abandoned-close [current-results current-token]
  (case (:id current-token)
    :codeblock (conj current-results {:start (:idx current-token)
                                      :end (+ (:idx current-token) (:length current-token))
                                      :id :code
                                      :children-start (inc (:idx current-token))
                                      :children-end (dec (+ (:idx current-token) (:length current-token)))
                                      :content (apply str (repeat (- (:length current-token) 2) "`"))})
    current-results))

(defn process-paired [injections token-key parser-parameters parser-state]
  (let [close-delimiter  (:close-delimiter injections)]
    (loop [state (assoc parser-state :pending-series-tokens (get-in parser-state [:pending-tokens token-key])
                        :pending-tokens (dissoc (:pending-tokens parser-state) token-key)
                           ; :elements (get parser-state :elements)
                        :free-openers [])]
      (let [these-pending-tokens (get state :pending-series-tokens)]
        (if (pos? (count these-pending-tokens))
          (let [current-token (nth these-pending-tokens 0)
                {:keys [direction]} current-token
                next-state (update state :pending-series-tokens subvec 1)]
            (recur (if (= :open direction)
                     (update next-state :free-openers
                             conj current-token)
                     ;; found potential closer
                     (if-let [partner (peek (:free-openers state))]
                       ;; found a matching pair, add to results
                       (close-delimiter parser-parameters state current-token partner)
                       ;; found a (potential) closer but no partner found
                       (if (= :close direction)
                         ;; close -> see if anything should be done with orphaned closer
                         (update next-state :elements
                                 on-abandoned-close current-token)
                         ;; dual -> add as opener
                         (update next-state :free-openers
                                 conj current-token))))))
          ;; no more tokens of this type to process -> return
          state)))))
