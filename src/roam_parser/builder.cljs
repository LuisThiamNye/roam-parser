(ns roam-parser.builder (:require [clojure.string]
                                  [roam-parser.utils :as utils]
                                  [roam-parser.rules :as rules]
                                  [roam-parser.elements :as elements]
                                  [roam-parser.tokens.token :as tkn]
                                  [roam-parser.tokens.protocols :refer [process-tokens]]))

(defn probe [x] (.log js/console x) x)

(def str-replace-re (js/RegExp. (str "\\\\(?<escape>" (utils/re-to-str rules/escapable-char-regex) ")")
                                "g"))
(defn escape-str [^string string] (.replace string str-replace-re "$<escape>"))

(defn get-sub
  ([^string string start end no-escape]
   (subs string start end))
  ([^string string start end]
   (-> (subs string start end)
       escape-str)))

(defn get-match [^string string re start end]
  (re-find re (subs string start end)))

(defn filter-tokens [f tks]
  (loop [output  tks
         tseq (seq tks)]
    (if-let [entry (first tseq)]
      (recur (assoc output (nth entry 0) (utils/eager-filter f (nth entry 1)))
             (next tseq))
      output)))

(defn find-token [tks k idx] (some #(when (identical? (:idx %) idx) %) (get tks k)))

(defn find-element [all-elements start-idx]
  (some #(when (= start-idx (:start %)) %) all-elements))
(defn find-loose-children [all-elements start-idx end-idx]
  (utils/eager-filter #(< start-idx (:start %) end-idx) all-elements))
(defn find-sibling-elements [all-elements start-idx end-idx]
  (utils/eager-filter #(not (< start-idx (:start %) end-idx)) all-elements))

;; keeps tokens outside of a range
(defn find-sibling-tokens
  ([tokens start-idx end-idx]
   (filter-tokens #(not (< start-idx (:idx %) end-idx)) tokens))
  ([tokens start-idx end-idx type-to-remove]
   (filter-tokens #(not (and (instance? type-to-remove %) (< start-idx (:idx %) end-idx))) tokens)))
(defn find-children-tokens [tokens start-idx end-idx]
  (filter-tokens #(<= start-idx (:idx %) end-idx) tokens))

(defn killed-by? [parent child]
  (contains? (elements/killed-by parent) (type child)))

(defn contains-newline? [^string string]
  (re-find #"\n" string))

(defn add-element
  "Adds the element and discards any pre-existing overlappings"
  [state {:keys [end start] :as el}]
  (assoc state :elements (-> (:elements state)
                             (find-sibling-elements start end)
                             (conj el))))

(declare process-children)

(defn add-element-with-children [state {:keys [end start] :as el} {:keys [parser-parameters children-tokens]}]
  {:pre (contains? el :children-end)}
  (if ((:el-type-allowed? parser-parameters) (type el))
    (let [el-with-children (process-children (assoc parser-parameters
                                                   :tokens (find-children-tokens children-tokens
                                                                                 (:children-start el) (:children-end el))
                                                   :parent el
                                                   :el-type-allowed? #(contains? (elements/allowed-children el) %)))]
      (if (nil? el-with-children)
        ;; element has been killed by a child and can't exist so do nothing
        state

       (-> state
           (add-element el-with-children)
           (update :pending-tokens find-sibling-tokens start end))))
    ;; this element is not allowed under the current parent, so do nothing
    state))

(defn finalise-children [parent ^string string init-elements mode]
  (let [conj-text-node (case mode
                         :insert-text (fn [coll start end]
                                        (cond-> coll
                                          (< start end) (conj (elements/->Text (get-sub string start end)))))
                         :no-text (fn [coll _ _] coll))]
    (loop [els (sort-by :start init-elements)
           section-start (:children-start parent)
           output []]
      (if (pos? (count els))
        (let [this-el (first els)]
          (if (killed-by? parent this-el)
            nil
            (recur (next els)
                   (:end this-el)
                   (-> output
                       (conj-text-node section-start (:start this-el))
                       (conj this-el)))))
        (assoc parent :children
               (conj-text-node output section-start (:children-end parent)))))))


;;
;;
;; close a pair of delimiters
;;




(defn update-token [tokens k idx f]
  (let [tkn-vec (get tokens k)
        tkn-count (dec (count tkn-vec))]
    (loop [i tkn-count]
      (if (neg? i)
        ;; did not find anything for the index provided
        tokens
        (let [tkn (nth tkn-vec i)]
          (if (<= (:idx tkn) idx)
            (assoc-in tokens [k i] (f tkn))
            (recur (dec i))))))))

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
                            (update-token (type current-token) (:idx current-token)
                                          (fn [tkn] (if (not= (:idx current-token) (:idx tkn))
                                                      (update tkn :length - (:length current-token)) tkn)))

                             ;; split up excess openers
                            (update-token (type current-token) (:idx partner)
                                          #(if (not= (:length partner) (:length %))
                                             (-> %
                                                 (update :length - (:length partner))
                                                 (update :idx + (:length partner)))
                                             %)))]
    (hash-map :quantity quantity
              :start-idx start-idx
              :end-idx end-idx
              :inner-start inner-start
              :inner-end inner-end
              :children-tokens children-tokens
              :add-bracket-element-with-children #(add-element-with-children %1 (assoc %2
                                                                                       :children-start inner-start
                                                                                       :children-end inner-end)
                                                                             (hash-map :parser-parameters parser-parameters
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
          (let [alias-id (if (find-token pending-tokens tkn/Bang (dec start-idx))
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


                  (assoc :pending-tokens (-> (find-sibling-tokens pending-tokens start-idx idx)
                                             (assoc-in [tkn/Round following-round-vec-idx :tag] alias-id))))))

          ;; case: this IS a [[page]]
          could-be-page
          (let [brackets? (find-token pending-tokens tkn/Hash (dec start-idx))
                base-data {:end end-idx
                           :start (cond-> start-idx brackets? dec)
                           :page-name (get-sub (:block-string parser-parameters) (+ start-idx quantity) (- end-idx quantity))}]
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
                         [:block-ref (get-sub (:block-string parser-parameters) inner-start inner-end :no-escape)]

                         (and (identical? quantity 1)
                              (or not-empty (= :alias (:tag partner))))
                         (let [children (:children (process-children (assoc parser-parameters
                                                                            :tokens (find-children-tokens (:tokens parser-parameters) (:idx partner) idx)
                                                                            :parent (elements/map->AliasDestinationVirtual
                                                                                     {:children-start (inc (:idx partner))
                                                                                      :children-end (dec idx)}))))
                               first-child (first children)]
                           (if (and (identical? 1 (count children))
                                    (instance? elements/PageLink first-child)
                                    (= inner-start (:start first-child))
                                    (= inner-end (:end first-child)))
                             [:page (:page-name first-child)]
                             [:url (get-sub (:block-string parser-parameters) inner-start inner-end)])))]
            (when dest-type
              (add-element-with-children current-state
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
                  (assoc :elements (:children (process-children (assoc parser-parameters
                                                                       :tokens (find-children-tokens (:tokens parser-parameters) (:idx partner) idx)
                                                                       :t-seq-order []
                                                                       :parent (assoc (:parent parser-parameters) :children-end inner-end :children-start inner-start)
                                                                       :text-mode :no-text))))
                  (assoc :pending-tokens (find-sibling-tokens (:pending-tokens current-state) start-idx idx tkn/Url))))))
        current-state)))

(defn close-curly [parser-parameters initial-state {:keys [idx] :as current-token} partner]
  (let [{:keys [quantity end-idx start-idx
                add-bracket-element-with-children
                not-empty current-state]} (get-close-bracket-data parser-parameters initial-state current-token partner)
        {:keys [free-openers]} current-state]
    (if (and not-empty (zero? (count free-openers))
             (>= quantity 2))
      (add-bracket-element-with-children current-state
                                         (elements/map->Render
                                          {:start (- (+ (:idx partner) (:length partner)) 2)
                                           :end (+ idx 2)}))
      current-state)))

(defn close-ambiguous-fn [min-delimiter-length add-extra]
  (fn [parser-parameters current-state current-token partner]
    (let [{:keys [length idx]} current-token
          end-idx                 (+ idx length)
          start-idx               (:idx partner)
          inner-start             (+ min-delimiter-length start-idx)
          inner-end               (- end-idx min-delimiter-length)]
      (-> current-state
          (update :free-openers pop)
          (update :pending-series-tokens subvec 1)
          (add-element-with-children (add-extra (hash-map :start start-idx
                                                          :end   end-idx
                                                          :children-start inner-start
                                                          :children-end inner-end)
                                                parser-parameters)
                                     (hash-map :parser-parameters parser-parameters
                                               :children-tokens (:tokens parser-parameters)))))))


;;
;;
;; single adders
;;
;;






;; TODO type


(defn process-children
  "Returns parent with children populated or nil if parent killed by child"
  [parser-parameters]
  {:pre (map? (:tokens parser-parameters))}
  (loop [token-seq-types (:t-seq-order parser-parameters)
         state {:elements []
                :pending-tokens (:tokens parser-parameters)}]
    (if (and (pos? (count token-seq-types)))
      (let [token-seq-type (nth token-seq-types 0)
            token-seq (token-seq-type.)]
          ;; TODO only allow some delimiters
        (if true
          (recur (subvec token-seq-types 1)
                 (process-tokens token-seq parser-parameters state))
          (recur (subvec token-seq-types 1) state)))
      (finalise-children (:parent parser-parameters) (:block-string parser-parameters) (:elements state) (:text-mode parser-parameters)))))
