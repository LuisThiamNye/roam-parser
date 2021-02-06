(ns roam-parser.rules.page
  (:require
   [clojure.string]
   [roam-parser.rules.relationships :refer [killed-by-of]]
   [roam-parser.rules.text-bracket :refer [start-text-bracket-fn]]
   [roam-parser.transformations :as transf]
   [roam-parser.elements :as elements]
   [roam-parser.state :refer [lookahead-contains?]]))

(defn terminate-page-link [state char]
  (when (identical? "]" char)
    (when (lookahead-contains? state "]")
      (let [idx     (:idx state)]
        (transf/ctx-to-element (:path state)
                               (fn [ctx]
                                 (let [page-name (subs (:string state) (:context/open-idx ctx) idx)]
                                   (when (and (not (identical? page-name ""))
                                              (identical? page-name (clojure.string/trim page-name)))
                                     ((if (:page-link/tag? ctx) elements/->BracketTag elements/->PageLink)
                                       page-name
                                       (:context/elements ctx)))))
                               {:context/id :context.id/page-link
                                :killed-by (killed-by-of :context.id/page-link) ;; TODO use polymorphism? implement killed-by?
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
