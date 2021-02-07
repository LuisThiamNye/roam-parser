(ns roam-parser.rules.page
  (:require
   [clojure.string]
   [taoensso.timbre :as t]
   [roam-parser.utils :refer [no-blank-ends?]]
   [roam-parser.rules.relationships :refer [killed-by-of]]
   [roam-parser.rules.text-bracket :refer [start-text-bracket-fn]]
   [roam-parser.transformations :as transf]
   [roam-parser.elements :as elements]
   [roam-parser.state :refer [lookahead-contains?]]))

(defn terminate-page-link [state char]
  (when (identical? "]" char)
    (when (lookahead-contains? state "]")
      (let [idx     (:idx state)]
        (transf/ctx-to-element (:roam-parser.state/path state)
                               (fn [ctx]
                                 (let [page-name (subs (:string state) (:context/open-idx ctx) idx)]
                                   (when (no-blank-ends? page-name)
                                     ((if (:page-link/tag? ctx) elements/->BracketTag elements/->PageLink)
                                      page-name
                                      (:context/elements ctx)))))
                               {:context/id :context.id/page-link
                                :killed-by (killed-by-of :context.id/page-link)
                                :next-idx   (+ 2 idx)})))))

(defn start-page-link [state char]
  (when (identical? "[" char)
    (let [double? (lookahead-contains? state "[")]
      (when double?
        (transf/try-new-ctx {:context/id        :context.id/page-link
                             :page-link/tag? false
                             :context/open-idx  (-> state :idx (+ 2))
                             :context/elements  []
                             :context/text-rules [(start-text-bracket-fn "[" "]")]
                             :context/killed-by (killed-by-of :context.id/page-link)
                             :context/terminate terminate-page-link}
                            state)))))

(defn insert-tag [state]
  (t/debug "INSERTING tag before the close")
  (let [ctx (-> state :roam-parser.state/path peek)
        idx (:idx state)
        page-name (subs (:string state) (:context/open-idx ctx) idx)]
    (when-not (identical? "" page-name)
      (assoc state :roam-parser.state/path (-> (:roam-parser.state/path state)
                             pop
                             (transf/add-element (elements/->Tag page-name)
                                                 state
                                                 (:context/start-idx ctx)
                                                 idx))))))

(defn terminate-tag [_ char]
  (when (nil? (re-matches #"[\w-_@.:*]" char))
    (fn [state _] (transf/fallback-from-last state))))

(defn start-tag [state char]
  (when (identical? \# char)
    (if (lookahead-contains? state "[[")
      (transf/try-new-ctx {:context/id        :context.id/page-link
                           :page-link/tag? true
                           :context/open-idx  (-> state :idx (+ 3))
                           :context/elements  []
                           :context/text-rules [(start-text-bracket-fn "[" "]")]
                           :context/killed-by (killed-by-of :context.id/page-link)
                           :context/terminate terminate-page-link}
                          state)
      (transf/try-new-ctx {:context/id        :context.id/tag
                           :context/open-idx  (-> state :idx inc)
                           :terminate-fallback insert-tag
                           :context/terminate terminate-tag}
                          state))))

(comment
  (simple-benchmark [] (clojure.string/starts-with? "    abc" \space) 10000)
;; 3
  (simple-benchmark [] (clojure.string/blank? "    abc") 10000)
;; 4
  (simple-benchmark [] (identical? \space (nth "    abc" 0)) 10000)
;; 7
  (simple-benchmark [] (clojure.string/trim "    abc") 10000)
  ;; 3
  (simple-benchmark [] (identical? "" "abc") 10000)
  ;; 0
  (simple-benchmark [] (str) 10000)
  ;; 0
  (def page-name "    abc ")

  (simple-benchmark [] (and (not (identical? page-name ""))
                            (identical? page-name (clojure.string/trim page-name)))
                    10000)
  ;; 2

  ;;;;;;;;;;
  )
