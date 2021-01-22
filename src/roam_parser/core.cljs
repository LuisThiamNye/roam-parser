(ns roam-parser.core (:require [clojure.set]
                               [clojure.string]
                               [goog.string]
                               [roam-parser.utils :as utils]
                               [roam-parser.render :as render]
                               [roam-parser.rules :as rules]
                               [roam-parser.builder :as builder]
                               [roam-parser.elements :as el]))

(defn parse-block [^string string]
  (builder/find-elements string))

(defn replace-node [f coll new]
  (let [idxs (keep-indexed #(when (f %2) %1) coll)
        idx (first idxs)]
    (assoc coll idx new)))

(defn replace-in [tree path new]
  (update-in tree path (fn [old] new)))

(defn stringify [element] el/stringify)
