(ns roam-parser.core (:require [clojure.set]
                               [clojure.string]
                               [goog.string]
                               [roam-parser.utils :as utils]
                               [roam-parser.render :as render]
                               [roam-parser.builder :as builder]
                               [roam-parser.rules :as rules]
                               [roam-parser.elements :as el]
                               [roam-parser.tokens.core :as tokens]
                               [roam-parser.builder.block-level :as bl]))


(defn parse-inline [^string string]
  (bl/process-block-level {:parent (el/map->Block {:children-start 0
                                                   :children-end (.-length string)})
                           :block-string string
                           :el-type-allowed? (fn [_] true)
                           :t-seq-order tokens/t-seq-order
                           :text-mode :insert-text}))

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
