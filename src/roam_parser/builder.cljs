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
  (into (hash-map) (map (juxt key (partial utils/eager-filter f)) tks)))

(defn find-token [tks k idx] (some #(when (identical? (:idx %) idx) %) (get tks k)))

(defn find-element [all-elements start-idx]
  (some #(when (= start-idx (:start %)) %) all-elements))
(defn find-loose-children [all-elements start-idx end-idx]
  (utils/eager-filter #(<= start-idx (:start %) (dec end-idx)) all-elements))
(defn find-sibling-elements [all-elements start-idx end-idx]
  (utils/eager-filter #(not (<= start-idx (:start %) (dec end-idx))) all-elements))

;; keeps tokens outside of a range
(defn find-sibling-tokens
  ([tokens start-idx end-idx]
   (filter-tokens #(not (<= start-idx (:idx %) (dec end-idx))) tokens))
  ([tokens start-idx end-idx type-to-remove]
   (filter-tokens #(not (and (instance? type-to-remove %) (<= start-idx (:idx %) (dec end-idx)))) tokens)))
(defn children-token-pred [start-idx end-idx]
  #(<= start-idx (:idx %) (dec end-idx)))
(defn find-children-tokens [tokens start-idx end-idx]
  (filter-tokens (children-token-pred start-idx end-idx) tokens))

(defn killed-by? [parent child-type]
  (contains? (elements/killed-by parent) child-type))

(defn contains-newline? [^string string]
  (re-find #"\n" string))

(defn add-element
  "Adds the element and discards any pre-existing overlappings"
  [state {:keys [end start] :as el}]
  (assoc state :elements (-> (:elements state)
                             (find-sibling-elements start end)
                             (conj el))))

  ;; keep the element as child if it is allowed or has the potential to kill the parent
(defn child-allowed? [el parser-parameters]
  (or ((:el-type-allowed? parser-parameters) (type el))
      (killed-by? (:parent parser-parameters) (type el))))

(declare process-children)

(defn add-element-with-children [state {:keys [end start] :as el} {:keys [parser-parameters
                                                                          children-tokens
                                                                          override-allowed-rules
                                                                          no-children?]}]
  {:pre (contains? el :children-end)}
  (if (child-allowed? el parser-parameters)
    (let [el-with-children (process-children (assoc parser-parameters
                                                    :tokens (find-children-tokens children-tokens
                                                                                  (:children-start el) (:children-end el))
                                                    :parent el
                                                    :el-type-allowed? (if override-allowed-rules
                                                                        #(contains? (elements/allowed-children el) %)
                                                                        #(and ((:el-type-allowed? parser-parameters) %)
                                                                              (contains? (elements/allowed-children el) %)))))]
      (if (nil? el-with-children)
        ;; element has been killed by a child and can't exist so do nothing
        state

        (-> state
            (add-element (cond-> el-with-children
                           no-children? (dissoc :children)))
            (update :pending-tokens find-sibling-tokens start end))))
    state))

(defn- finalise-children [parent ^string string init-elements mode]
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
          (if (killed-by? parent (type this-el))
            nil
            (recur (next els)
                   (:end this-el)
                   (-> output
                       (conj-text-node section-start (:start this-el))
                       (conj this-el)))))
        (assoc parent :children
               (conj-text-node output section-start (:children-end parent)))))))




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




(defn process-children
  "Returns parent with children populated or nil if parent killed by child"
  [parser-parameters]
  {:pre (map? (:tokens parser-parameters))}
  (loop [token-seq-types (:t-seq-order parser-parameters)
         state           {:elements       []
                          :pending-tokens (:tokens parser-parameters)}]
    (if (and (pos? (count token-seq-types)))
      (let [token-seq-type (nth token-seq-types 0)
            token-seq      (token-seq-type.)]
        ;; TODO only allow some delimiters
        (if true
          (recur (subvec token-seq-types 1)
                 (process-tokens token-seq parser-parameters state))
          (recur (subvec token-seq-types 1) state)))
      (finalise-children (:parent parser-parameters) (:block-string parser-parameters) (:elements state) (:text-mode parser-parameters)))))

;; TODO
(defn fallback [context]
  nil)

(defn process-char [state char string]

  (let [idx (:idx state)]
    (case char
      "[" (let [context-order [:context/page-link :context/alias-square :context/text-square]
                double?       (identical? "[" (nth string (inc idx)))
                alias-square  {:context/id       :context/alias-square
                               :open-idx         (inc idx)
                               :context/elements []
                               :context/fallback {:context/id       :context/text-square
                                                  :open-idx         (inc idx)
                                                  :context/elements []
                                                  :context/fallback nil}}]
            (if double?
              (let [next-idx (+ 2 idx)]
                (-> state
                    (update :path conj {:context/id       :context/page-link
                                        :open-idx         next-idx
                                        :context/elements []
                                        :context/fallback alias-square})
                    (assoc :idx next-idx)))
              (let [next-idx (inc idx)]
                (-> state
                    (update  :path conj alias-square)
                    (assoc :idx next-idx)))))
      "]" (let [path       (:path state)
                last-index (dec (count path))]
            (loop [i last-index]
              (if (neg? i)
                (update state :idx inc)
                (case (:context/id (nth path i))
                  :context/page-link    (if (identical? "]" (nth string (inc idx)))
                                          (if (< i last-index)
                                            (let [failed-context   (nth path (inc i))
                                                  fallback-context (fallback failed-context)]
                                              (-> state
                                                  (assoc :path (conj (subvec path 0 (inc i))
                                                                     fallback-context))
                                                  (assoc :idx (:open-idx (or fallback-context failed-context)))))
                                            (let [partner (peek path)]
                                              (-> state
                                                  (assoc :path (-> path
                                                                   pop
                                                                   (update-in [(dec last-index) :context/elements]
                                                                              conj
                                                                              (elements/->PageLink (subs string (:open-idx partner) idx)
                                                                                                   (:context/elements partner)))))
                                                  (update :idx + 2))))
                                          (update state :idx inc))
                  :context/alias-square (update state :idx inc)
                  :context/text-square  (update state :idx inc)
                  (recur (dec i))))))
      (update state :idx inc))))

(defn find-elements [string]
  (let [str-length (count string)]
    (loop [state {:path [{:context/id       :context/block
                          :open-idx         0
                          :context/elements []}]
                  :idx  0}]
      (let [idx (:idx state)]
        (if (< idx str-length)
          (let [char (nth string idx)]
            (recur (process-char state char string) ))
          (-> state :path (nth 0) :context/elements
              #_ (conj (elements/->Text (subs string (-> state :elements peek :end))))))))))

(comment
  (find-elements "[[[[a]]d[[b]]]]hhlolol")
  (simple-benchmark []   (find-elements "[[[[a]]d[[b]]]]hhlolol") 10000                    )



  ;; bracket run length lookahead
  ;; probably better to not as assumption works better
  #_:clj-kondo/ignore
  (let [char-count (count (re-find #"\[*" (subs string idx)))
        n-doubles  (quot char-count 2)
        a-single?  (pos? (mod char-count 2))]
    (loop [n n-doubles
           s state]
      ))


  (simple-benchmark [] {:context/id       :context/alias-square
                        :open-idx         (inc 54)
                        :context/elements []} 10000)
  ;; 7

  (simple-benchmark [] (rand-nth ["a" "b" "c"]) 1000000)
  ;; 40
  (simple-benchmark [  ] (case (rand-nth ["a" "b" "c"]) "b" 5 "c" 8 "a" 3 0) 1000000)
  ;; 45 - 40 = 5
  (simple-benchmark [  ] (case (rand-nth [:a :b :c]) :b 5 :c 8 :a 3 0) 1000000)
  ;; 101 - 40 =60
  (simple-benchmark [x {"b" 5 "c" 8 "a" 3}] (get x "a") 10000)
  ;; 9


  (simple-benchmark [] (= "a" "b") 100000)
  ;; 13
  (simple-benchmark [] (identical? "a" "b") 100000)
  ;; 1
  (simple-benchmark [] (case "a" "b" true false) 100000)
  ;; 1

  (defn some-recur
    ([f]
     (fn [i]
       (if (> i 15)
         "new state"
         (f (inc i))))))

  (def stak (apply comp (take 15 (repeat some-recur))))
  (simple-benchmark[] (comp some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur ) 10000)
  ;; 40

  (def active (stak identity))

  (simple-benchmark [](active 2) 100000)
  ;; 16

  (defn try-smth [i]
    (if (> i 15)
      "new state"
      nil))

  (simple-benchmark []
                    (loop [i 1]
                      (if (nil? (try-smth i))
                        (recur (inc i))
                        "yes")) 100000)
  ;; 16

  (simple-benchmark [] (let [[a b] [{:a :map} true]]
                         [a b]) 100000)
  ;; 23


  )
