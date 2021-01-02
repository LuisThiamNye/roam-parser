(ns roam-parser.builder (:require [clojure.string]
                                  [roam-parser.utils :as utils]
                                  [roam-parser.rules :as rules]
                                  [roam-parser.elements :as elements]
                                  [roam-parser.tokens :as tokens]))

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
  (loop [x  tks
         ks (keys tks)]
    (if-let [tk-key (first ks)]
      (recur (update x tk-key (partial utils/eager-filter f))
             (next ks))
      x)))

(defn find-token [tks id idx] (some #(when (identical? (:idx %) idx) %) (get tks id)))

(defn find-loose-children [all-elements start-idx end-idx]
  (utils/eager-filter #(< start-idx (:start %) end-idx) all-elements))
(defn find-sibling-elements [all-elements start-idx end-idx]
  (utils/eager-filter #(not (< start-idx (:start %) end-idx)) all-elements))

;; keeps tokens outside of a range
(defn find-sibling-tokens
  ([tokens start-idx end-idx]
   (filter-tokens #(not (< start-idx (:idx %) end-idx)) tokens))
  ([tokens start-idx end-idx id-to-remove]
   (filter-tokens #(not (and (= id-to-remove (:id %)) (< start-idx (:idx %) end-idx))) tokens)))
(defn find-children-tokens [tokens start-idx end-idx]
  (filter-tokens #(<= start-idx (:idx %) end-idx) tokens))

(defn killed-by? [parent child]
  ;;TODO
  nil)

(defn contains-newline? [^string string]
  (re-find #"\n" string))

(defn add-element  [state {:keys [end start] :as el}]
  (assoc state :elements (-> (:elements state)
                             (find-sibling-elements start end)
                             (conj el))))

(declare process-children)

(defn add-element-with-children [state {:keys [end start] :as el} {:keys [parser-parameters
                                                                          pending-tokens]}]
  {:pre (contains? el :children-end)}
  (let [el-with-children (process-children el
                                           (update parser-parameters :tokens
                                                   find-children-tokens (:children-start el) (:children-end el)))]
    (if (nil? el-with-children)
      state
      (-> state
          (add-element el-with-children)
          (update :pending-tokens find-sibling-tokens start end)))))

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
;;


(defn add-opener
  ([openers new] (conj openers {:idx (:idx new) :length (:length new)}))
  ([openers new tag] (conj openers {:idx (:idx new) :length (:length new) :tag tag})))

(defn close-bracket-data [parser-parameters initial-state current-token partner]
  (let [{:keys [length idx]} current-token
        quantity (min (:length partner) length)
        start-idx (- (+ (:idx partner) (:length partner)) quantity)
        end-idx (+ idx quantity)
        inner-end (- end-idx quantity)
        inner-start (+ start-idx quantity)]
    (hash-map :quantity quantity
              :start-idx start-idx
              :end-idx end-idx
              :inner-start inner-start
              :inner-end inner-end
              :add-bracket-element #(add-element %1 %2)
              :add-bracket-element-with-children #(add-element-with-children %1 (assoc %2
                                                                                       :children-start inner-start
                                                                                       :children-end inner-end)
                                                                             (hash-map :parser-parameters parser-parameters
                                                                                       :pending-tokens (:pending-tokens initial-state)))
              :not-empty (pos? (- inner-end inner-start))
              :current-state (as-> initial-state state
                               ;; add excess closers to the queue to be processed next
                               (cond-> state (> length quantity)
                                 (update state :these-pending-tokens #(into (vector (-> current-token
                                                                                        (update :length - quantity)
                                                                                        (update :idx + quantity)))
                                                                            %)))
                               ;; split opener into higher level, outer opener if long enough
                               (if (> (:length partner) quantity)
                                 (update state :free-openers assoc (-> state :free-openers count dec) (update partner :length - quantity))
                                 (update state :free-openers pop))))))

(defn close-square [parser-parameters _ initial-state {:keys [id idx] :as current-token} partner]
  (let [{:keys [quantity end-idx start-idx add-bracket-element-with-children
                add-bracket-element
                not-empty current-state
                loose-children]} (close-bracket-data parser-parameters initial-state current-token partner)
        {:keys [pending-tokens]} current-state
        round-tks (:round pending-tokens)
        following-round-idx (loop [i (dec (count round-tks))]
                              (if (neg? i)
                                nil
                                (let [tkn (nth round-tks i)]
                                  (if (and (= end-idx (:idx tkn)) (= :open (:direction tkn)))
                                    i
                                    (recur (dec i))))))
        could-be-page (> quantity 1)]
    (or (cond
          ;; case: this is potential [alias text]()
          (some? following-round-idx)
          (let [alias-id (if (find-token pending-tokens :bang (dec start-idx))
                           :image
                           :alias)]
            (when (or not-empty (not (= :alias alias-id)))
              (-> current-state
                  (add-bracket-element {:end end-idx
                                        :start start-idx
                                        :id id
                                        :children-start (+ start-idx 1)
                                        :children-end (- end-idx 1)
                                        :deferred-data {:loose-children loose-children
                                                        :delimiter-queue (subvec (:delimiter-queue parser-parameters) 1)
                                                        :pending-tokens pending-tokens}})
                                  ;; mark opening round as potential alias
                  (assoc :pending-tokens (-> (find-sibling-tokens pending-tokens start-idx idx)
                                             (assoc-in [:round following-round-idx :tag] alias-id))))))

          ;; case: this IS a [[page]]
          could-be-page
          (let [page-id (if (find-token pending-tokens :hash (dec start-idx))
                          :bracket-tag
                          :page)]
            (when not-empty
              (add-bracket-element-with-children
               current-state
               (-> {:end end-idx
                    :start (cond-> start-idx
                             (= :bracket-tag page-id) dec)
                    :id page-id
                    :page (get-sub (:block-string parser-parameters) (+ start-idx quantity) (- end-idx quantity))}
                   (elements/map->PageLink))))))
        current-state)))

(defn close-round [parser-parameters parent initial-state {:keys [idx] :as current-token} partner]
  (let [{:keys [quantity end-idx start-idx
                inner-start loose-children
                inner-end not-empty add-bracket-element-with-children
                current-state]} (close-bracket-data parser-parameters initial-state current-token partner)
        {:keys [elements pending-tokens]} current-state]
    (or (if (contains? #{:alias :image} (:tag partner))

          ;; make an alias
          (let [[square-pair
                 square-pair-idx-in-elements] (loop [i (dec (count elements))]
                                                (if (neg? i)
                                                  nil
                                                  (let [el (nth elements i)]
                                                    (if (identical? (:idx partner) (:end el))
                                                      [el i]
                                                      (recur (dec i))))))
                square-data (:deferred-data square-pair)
                element-id (:tag partner)
                [dest-type
                 dest] (cond
                         (and (identical? quantity 3) not-empty)
                         [:block-ref (get-sub (:block-string parser-parameters) inner-start inner-end :no-escape)]

                         (and (identical? quantity 1)
                              (or not-empty (= :alias (:tag partner))))
                         (let [children (:children (process-children {:id :alias-destination
                                                                      :children-start (inc (:idx partner))
                                                                      :children-end (dec idx)}
                                                                     (update parser-parameters :tokens
                                                                             find-children-tokens (:idx partner) idx)))
                               first-child (first children)]
                           (if (and (identical? 1 (count children))
                                    (= :page (:id first-child))
                                    (= inner-start (:start first-child))
                                    (= inner-end (:end first-child)))
                             [:page (:page first-child)]
                             [:url (get-sub (:block-string parser-parameters) inner-start inner-end)])))]
            (when dest-type
              (add-element-with-children current-state
                                         (cond-> {:start (cond-> (:start square-pair)
                                                           (= :image element-id) dec)
                                                  :divider-idx (dec (:end square-pair))
                                                  :end end-idx
                                                  :id element-id
                                                  :children-start (:children-start square-pair)
                                                  :children-end (:children-end square-pair)
                                                  :destination-type dest-type
                                                  :destination dest}
                                           (= :image element-id) (elements/map->Image)
                                           (= :alias element-id) (elements/map->Alias))
                                         (hash-map :pending-tokens (:pending-tokens square-data)
                                                   :parser-parameters (assoc parser-parameters :delimiter-queue
                                                                             (:delimiter-queue square-data))))))

          ;; (()) block ref or parenthetical
          (when not-empty
            (if (> quantity 1)
              (add-bracket-element-with-children current-state (-> {:start start-idx
                                                                    :end end-idx
                                                                    :id :parenthetical}
                                                                   (elements/map->Parenthetical)))

              ;;  a normal pair of ( ) brackets, discard and surface children as an isolated bunch
              (-> current-state
                  (assoc :elements (:children (process-children (assoc parent :children-end inner-end :children-start inner-start)
                                                                (assoc (update parser-parameters :tokens
                                                                               find-children-tokens (:idx partner) idx)
                                                                       :delimiter-queue [:url]
                                                                       :text-mode :no-text))))
                  (update :pending-tokens find-sibling-tokens start-idx idx :url)))))
        current-state)))

(defn close-curly [parser-parameters _ initial-state {:keys [id idx] :as current-token} partner]
  (let [{:keys [quantity end-idx start-idx
                add-bracket-element-with-children
                not-empty current-state]} (close-bracket-data parser-parameters initial-state current-token partner)
        {:keys [free-openers]} current-state]
    (if not-empty
      (add-bracket-element-with-children current-state
                                            (if (and (zero? (count free-openers))
                                                     (>= quantity 2))
                                              {:start (- (+ (:idx partner) (:length partner)) 2)
                                               :end (+ idx 2)
                                               :id :render}
                                              {:start start-idx
                                               :end end-idx
                                               :id id}))
      current-state)))

(defn close-ambiguous [delimiter-length parser-parameters _ {:keys [pending-tokens] :as current-state} current-token partner]
  (let [{:keys [id length idx]} current-token
        end-idx                 (+ idx length)
        start-idx               (:idx partner)
        inner-start             (+ delimiter-length start-idx)
        inner-end               (- end-idx delimiter-length)]
    (add-element-with-children (update current-state :free-openers pop)
                               (as-> (hash-map :start start-idx
                                               :end   end-idx
                                               :id    id
                                               :children-start inner-start
                                               :children-end inner-end)
                                     e
                                 (case id
                                   :code      (assoc e :content (get-sub (:block-string parser-parameters) inner-start inner-end))
                                   :codeblock (let [[_ lang content] (->> (get-sub (:block-string parser-parameters) inner-start inner-end)
                                                                          (.exec rules/codeblock-re))]
                                                (assoc e :language (if (nil? lang) rules/default-codeblock-lang lang)
                                                       :content  content))
                                   :latex     (assoc e :content (get-sub (:block-string parser-parameters) inner-start inner-end))
                                   ;; default: formatting
                                   e))
                               (hash-map :parser-parameters parser-parameters
                                         :pending-tokens pending-tokens))))


;;
;;
;; single adders
;;
;;







;; TODO type
(defn default-add-single-element [state current-token]
  (update state :elements conj {:start (:idx current-token)
                                :end (+ (:idx current-token) (:length current-token))}))




(defn process-children [token-seq parser-parameters]
  (let []
    (loop [token-seq-types (:token-seq-type-queue parser-parameters)
           element-acc []
           pending-tokens (:tokens parser-parameters)]
      (if (and (pos? (count token-seq-types)))
        (let [token-seq-type (nth token-seq-types 0)
              token-seq (token-seq-type. (get pending-tokens token-seq-type))
              other-pending-tokens (dissoc pending-tokens token-seq-type)]
          ;; TODO only allow some delimiters
          (if true
            (let [state (tokens/process-tokens token-seq parser-parameters other-pending-tokens element-acc)]
              (recur (subvec token-seq-types 1)
                     (:elements state)
                     (:pending-tokens state)))
            (recur (subvec token-seq-types 1) element-acc pending-tokens)))
        (finalise-children (:parent parser-parameters) (:block-string parser-parameters) element-acc (:text-mode parser-parameters))))))
