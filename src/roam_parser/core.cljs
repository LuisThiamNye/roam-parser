(ns roam-parser.core (:require [clojure.set]
                               [clojure.string]
                               [goog.string]
                               [roam-parser.utils :as utils]
                               [roam-parser.render :as render]
                               [roam-parser.builder :as builder]
                               [roam-parser.rules :as rules]
                               [roam-parser.tokens :as delimiters]))

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






(defn parse-inline [^string string]
  (let [matches (.matchAll string delimiters/inline-re)
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
        root-text-elements (fn root-text-elements []
                             (builder/process-children {:id :block
                                                        :children-start 0
                                                        :children-end (.-length string)}
                                                       {:tokens all-tokens
                                                        :block-string string
                                                        :delimiter-queue rules/rules
                                                        :text-mode :insert-text}))]
    (:children (if-let [hc (first (:hiccup all-tokens))]
       [{:id :hiccup
         :start 7
         :end (dec (.-length string))
         :children []}]
       (if-let [bq (first (:blockquote all-tokens))]
         [{:id :blockquote
           :start (:length bq)
           :end (dec (.-length string))
           :children (root-text-elements)}]
         (root-text-elements))))))

(defn replace-node [f coll new]
  (let [idxs (keep-indexed #(when (f %2) %1) coll)
        idx (first idxs)]
    (assoc coll idx new)))

(defn replace-in [tree path new]
  (update-in tree path (fn [old] new)))


(defn stringify-element [el]
  (case (:id el)
    :text (:content el)
    :hiccup
    :codeblock
    :code

    nil))

(defn js-add-element-strings! [arr els]
  (doseq [el els] (.push arr (stringify-element el))))

(defn stringify-block [block]
  (let [str-arr #js []]
    (js-add-element-strings! str-arr (:children block))
    (.join str-arr "")))
