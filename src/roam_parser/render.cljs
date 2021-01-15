(ns roam-parser.render (:require [clojure.string]
                                 [roam-parser.tokens.protocols :refer [TokenSequenceProtocol]]
                                 [roam-parser.elements :as elements :refer [ElementProtocol Codeblock Code PageLink]]
                                 [roam-parser.builder :as builder]
                                 [roam-parser.utils :as utils]
                                 [roam-parser.tokens.token :refer []]))

(def el-type-allowed?-textual (fn [_] false))

(def component-definitions {"query" {::arg-type :lisp}
                            "embed" {::arg-type :lisp}
                            "youtube" {::arg-type :textual}
                            "Δ" {::arg-type :hybrid}
                            "POMO" {::arg-type :none}
                            "roam/js" {::arg-type :none}})

(def delta-shorthand-re (js/RegExp. #"^(\d+(?:\.\d+)?)(?: *([-\+\*]) *(\d+(?:\.\d+)?))?$" ""))

(defn parse-float [^string string]
  (let [result (js/parseFloat string)]
    (if (js/isNaN result) nil result)))

(defn parse-delta-shorthand [^string string]
  (let [match (.exec delta-shorthand-re string)]
    (if (nil? match)
      nil
      (let [duration (parse-float (nth match 1))
            operation (case (nth match 2)
                        "-" -
                        "+" +
                        "/" /
                        "*" *)
            modifier (parse-float (nth match 3))]))))

(def base-re (js/RegExp. #"^(\[\[([\S]+?)\]\]|[\S]+?)(?::\s*(.*)?(?<!\s)|$)" "s"))
(def form-re (js/RegExp. #"[^\s,]+" "g"))

(defn get-forms [^string string]
  (reduce conj [] (.match string form-re)))



(defn init-render-element [^string block-string init-map]
  (let [string (builder/get-sub block-string (:children-start init-map) (:children-end init-map) :no-escape)
        match (.exec base-re string)]
    (if (nil? match)
      nil
      (let [name (nth match 1)
            page-name (nth match 2)
            content (nth match 3)
            [comp-name
             linked?] (if (nil? page-name)
                        [name false]
                        [page-name true])]
        (elements/map->Render (-> init-map
                                  (assoc :id comp-name
                                         :linked? linked?)
                                  (cond-> (= :textual (get-in component-definitions [comp-name ::arg-type]))
                                    (assoc :content content))
                                  (assoc :children-start (if (nil? content)
                                                           (:children-end init-map)
                                                           (+ 1 (count name) (:children-start init-map))))))))))

()
