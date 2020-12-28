(ns roam-parser.core (:require [clojure.set]
                               [clojure.string]
                               [goog.string]
                               [roam-parser.utils :as utils]
                               [roam-parser.render :as render]
                               [roam-parser.builder :as builder]
                               [roam-parser.rules :as rules]))

(defn probe [x] (.log js/console x) x)

(defn j-time
  ([name x]
   (if false
     (do (.log js/console name)
         (j-time x))
     (x)))
  ([x]
   (if false
     (do (.time js/console "time")
         (let [result (x)]
           (.timeEnd js/console "time")
           result))
     (x))))


(defn to-regex [^string str] (js/RegExp. str "gm"))
(def inline-re (to-regex (reduce #(str %1 "|" %2) (str "\\\\(?:" (utils/re-to-str rules/escapable-char-regex) ")")
                                 (map #(utils/re-to-str (:regex %))
                                      rules/delimiters))))




(defn parse-inline [^string string]
  (let [matches (.matchAll string inline-re)
        ;; map of delimiters and their occurrences
        all-tokens (dissoc (loop [output (transient (hash-map))]
                             (let [iter-item (.next matches)
                                   m (.-value iter-item)]
                               (if ^boolean (.-done iter-item)
                                 (persistent! output)
                                 (let [text (nth m 0)
                                       groups (.entries js/Object (.-groups m))
                                       group-count (.-length groups)
                                       group (loop [i 0]
                                               (if (< i group-count)
                                                 (let [[name value] (nth groups i)]
                                                   (if (nil? value)
                                                     (recur (inc i))
                                                     [name value]))
                                                    ;; does not match any group
                                                 [nil nil]))
                                       length (.-length text)
                                       idx (.-index m)
                                       group-name (nth group 0)
                                       symbol-data (if (nil? group-name)
                                                     (or (rules/symbol-data-from-char (nth text 0))
                                                         {:id :ignored})
                                                     (assoc (rules/symbol-data-from-group group-name)
                                                            :text (peek group)))
                                       new-token (merge symbol-data
                                                        {:length length
                                                         :idx idx})
                                       old-data (get output (:id symbol-data))]
                                   (recur (assoc! output (:id symbol-data)
                                                  (if (nil? old-data)
                                                    (vector new-token)
                                                    (conj old-data new-token))))))))
                           :ignored)
        add-text-nodes (fn add-text-nodes [elements start end]
                         (let [add-text-node (fn add-text-node [coll start end]
                                               (cond-> coll
                                                 (< start end) (conj {:id :text
                                                                      :content (builder/get-sub string start end)})))]
                           (loop [bounding-els (sort-by :start elements)
                                  section-start start
                                  output []]
                             (if (seq bounding-els)
                               (let [this-el (first bounding-els)]
                                 (recur (next bounding-els)
                                        (:end this-el)
                                        (-> output
                                            (add-text-node section-start (:start this-el))
                                            (conj this-el))))
                               (add-text-node output section-start end)))))
        phase-3 (fn phase-3 [parent start end allowed-children]
                  (let [parent-definition (get rules/element-definitions (:id parent))
                        children (reduce into (map (fn [c]
                                                     (if (:children c)
                                                       (let [child-definition (get rules/element-definitions (:id c))]
                                                         (phase-3 c (:children-start c) (:children-end c)
                                                                  (cond-> (:allowed-children child-definition)
                                                                    (not (:invisible? child-definition)) (clojure.set/intersection allowed-children))))
                                                       [c]))
                                                   (:children parent)))
                        ids (set (keep :id children))
                        killed? (or (when-let [killed-by (:killed-by parent-definition)]
                                      (some killed-by ids))
                                    (when (= :inline (:scope parent-definition))
                                      (seq (filter #(< (:start parent) (:idx %) (:end parent)) (:newline all-tokens)))))]
                    (if killed?
                      children
                      [(assoc parent :children (add-text-nodes (filter #(contains? allowed-children (:id %)) children)
                                                               start end))])))

        hit-tokens (dissoc (select-keys all-tokens (for [[k v] all-tokens :when (seq v)] k))
                           :blockquote :hiccup)
        process-elements (fn process-elements [elements]
                           (for [i elements]
                             (subs string (:start i) (:end i))))
        root-text-elements (fn root-text-elements
                             ([existing-els]
                              (add-text-nodes (builder/process-slice all-tokens
                                                                     {:block-string string
                                                                      :delimiter-queue rules/rules}
                                                                     {:id :block
                                                                      :children-start 0
                                                                      :children-end (.-length string)}
                                                                     existing-els)
                                              0 (.-length string)))
                             ([] (root-text-elements [])))]
    (if-let [hc (first (:hiccup all-tokens))]
      [{:id :hiccup
        :start 7
        :end (dec (.-length string))
        :children []}]
      (if-let [bq (first (:blockquote all-tokens))]
        [{:id :blockquote
          :start (:length bq)
          :end (dec (.-length string))
          :children (root-text-elements)}]
        (:children (first (let [x (root-text-elements)] (phase-3 {:id :block :children-start 0 :children-end (.-length string) :children x}
                                                                 0 (.-length string) (:allowed-children (get rules/element-definitions :block))))))))))

(defn replace-node [f coll new]
  (let [idxs (keep-indexed #(when (f %2) %1) coll)
        idx (first idxs)]
    (assoc coll idx new)))

(defn replace-in [tree path new]
  (update-in tree path (fn [old] new)))
