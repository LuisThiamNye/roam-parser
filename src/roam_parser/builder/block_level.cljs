(ns roam-parser.builder.block-level (:require [goog.object]
                                               [roam-parser.builder :as builder]
                                               [roam-parser.elements :as el]
                                               [roam-parser.utils :as utils]
                                               [roam-parser.tokens.core :as tokens]))
;; For elements dependent on the first token of a block

;;
;; mutability ahead
;;

(defn token-from-match [match]
  (let [text (nth match 0)
        groups (.entries js/Object (.-groups match))
        group-count (.-length groups)
        group (loop [i 0]
                (if (< i group-count)
                  (let [[name value] (nth groups i)]
                    (if (nil? value)
                      (recur (inc i))
                      [name value]))
                  ;; does not match any group
                  [nil nil]))
        idx (.-index match)
        group-name (nth group 0)]
    (if (nil? group-name)
      (tokens/token-from-text text idx)
      (tokens/token-from-group group-name (peek group) idx (count text)))))

(defn matches-to-tokens [matches init-map]
  (dissoc (loop [output (transient init-map)]
     (let [iter-item (.next matches)]
       (if ^boolean (.-done iter-item)
         (persistent! output)
         (let [token (token-from-match (.-value iter-item))
               old-tokens-of-type (get output (type token))]
           (recur (assoc! output (type token)
                          (if (nil? old-tokens-of-type)
                            (vector token)
                            (conj old-tokens-of-type token))))))))
          nil))

(defn initial-tokens [match]
  (let [t (token-from-match match)]
    (hash-map (type t) (vector t))))


(defn make-blockquote [text parser-parameters matches]
  (let [bq (el/map->BlockQuote {:link-type (case (subs text 0 1)
                                             \> :page-link-type/none
                                             \[ :page-link-type/bracket
                                             \# :page-link-type/bracket-tag)
                                :children-start (.-length text)
                                :children-end (:children-end (:parent parser-parameters))})]
    (assoc (:parent parser-parameters) :children
           (vector (builder/process-children
             (assoc parser-parameters
                    :parent bq
                    :tokens (matches-to-tokens matches (hash-map))
                    :el-type-allowed? #(contains? (el/allowed-children bq) %)))))))

(defn make-hiccup [content parser-parameters]
  (assoc (:parent parser-parameters)
         :children (vector (el/->Hiccup content))))


(defn process-block-level [parser-parameters]
  (let [matches ^js (.matchAll (:block-string parser-parameters) tokens/inline-re)
        iter-item (.next matches)
        match (.-value iter-item)]
    (if (.-done iter-item)
    ;; no tokens so no elements in block
      (assoc (:parent parser-parameters) :children
             (vector (el/->Text (:block-string parser-parameters))))

      (let [groups (.-groups match)]
        (cond
          (some? (.-blockquote groups)) (make-blockquote (.-blockquote groups) parser-parameters matches)
          (some? (.-hiccup groups)) (make-hiccup (.-hiccup groups) parser-parameters)
          :else (builder/process-children (assoc parser-parameters :tokens
                                                 (matches-to-tokens matches (initial-tokens match)))))))))
