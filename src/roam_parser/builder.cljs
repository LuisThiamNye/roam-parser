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
