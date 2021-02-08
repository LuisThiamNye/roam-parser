(ns roam-parser.rules.render
  (:require
   [clojure.string]
   [clojure.spec.alpha :as s]
   [taoensso.timbre :as t]
   [roam-parser.utils :as utils]
   [roam-parser.rules.relationships :refer [killed-by-of block-ctxs]]
   [roam-parser.rules.text-bracket :refer [start-text-bracket-fn]]
   [roam-parser.state :as state :refer [lookahead-contains? get-sub]]
   [roam-parser.elements :as elements]
   [roam-parser.transformations :as transf]
   [roam-parser.rules.render.components :as comps]))


(defn terminate-render [state char]
  (when (and (identical? "}" char)
             (lookahead-contains? state "}"))
    (let [idx     (:idx state)]
      (transf/ctx-to-element (:roam-parser.state/path state)
                             (fn [ctx]
                               (when-some [component (comps/render-comp ctx)]
                                 (elements/->Render
                                  (:render/id ctx)
                                  (:linked? ctx)
                                  (:context/elements ctx)
                                  component)))
                             {:context/id :context.id/render
                              :killed-by (killed-by-of :context.id/render)
                              :next-idx   (+ 2 idx)}))))

(defn render-id-data
  "Returns [linked? render-id]"
  [state]
  (let [ctx (-> state :roam-parser.state/path peek)
        els (:context/elements ctx)
        last-el (peek els)]
    (case (count els)
      0 (let [render-id (get-sub (:string state) (:context/open-idx ctx) (:idx state))]
          (when-not (clojure.string/blank? render-id)
            [false render-id]))
      1 (when (-> ctx :context/last-idx (identical? (:idx state)))
          [true (:page-name last-el)])
      nil)))

(defn terminate-render-id [state char]
  (cond (identical? char \:)
        (when-some [[linked? render-id] (render-id-data state)]
          (transf/swap-ctx (:roam-parser.state/path state)
                           (->(comps/configure-render-ctx
                             {:context/id         :context.id/render
                              :render/id          render-id
                              :linked?            linked?
                              :context/open-idx   (-> state :idx inc)
                              :context/elements   []
                              :context/text-rules [(start-text-bracket-fn  "{" "}")]
                              :context/killed-by  (killed-by-of :context.id/render)})
                              (update :context/extra-rules conj terminate-render))
                           {:context/id :context.id/render-id
                            :killed-by  (killed-by-of :context.id/render-id)}))

        (and (identical? char "}")
             (lookahead-contains? state "}"))
        (when-some [[linked? render-id] (render-id-data state)]
          (transf/ctx-to-element (:roam-parser.state/path state)
                                 (fn [_]
                                   (elements/->Render render-id linked? nil (comps/empty-comp render-id)))
                                 {:context/id :context.id/render-id
                                  :killed-by  (killed-by-of :context.id/render-id)
                                  :next-idx   (-> state :idx (+ 2))}))))

(defn start-render [state char]
  (when (identical? "{" char)
    (let [double? (lookahead-contains? state "{")]
      (when double?
        (transf/try-new-ctx {:context/id        :context.id/render-id
                             :context/open-idx (-> state :idx (+ 2))
                             :context/elements  []
                             :context/killed-by (killed-by-of :context.id/render-id)
                             :context/allows-ctx? #(contains? #{:context.id/page-link} %)
                             :context/terminate terminate-render-id}
                            state)))))

(comment
  (simple-benchmark [] (-> {}
                           (assoc :a 8)
                           (assoc :b 8)
                           (assoc :c 8))
                    10000)
  ;; 20, 10, 15, 8, 8

  (simple-benchmark [] (merge {}
                              {:a 8
                               :b 8
                               :c 8})
                    10000)
  ;; 28, 18, 28

  (simple-benchmark [x [8 8 8]] (-> {}
                                    (assoc :a (nth x 0))
                                    (assoc :b (nth x 1))
                                    (assoc :c (nth x 2)))
                    10000)
  ;; 16, 11, 11

  (simple-benchmark [] (let [[a b c] [1 2]]) 10000)
;; 10, 6, 7
  (simple-benchmark [] (let [[a b] [1 2]]) 10000)
;; 5
  (simple-benchmark [] (let [[a b c] [1 2 3]]) 10000)
  ;; 7, 6

  (simple-benchmark []  (clojure.string/split "ab | b" #"\|") 10000)
  ;; 22

  (s/check-asserts false)

  (defn x [get-block-string]
    (s/assert (s/fspec :args (s/cat :uid string?)
                       :ret string?)
              get-block-string))
  (x (fn [d] d 8))

  (s/assert string? 8)

  (def els ["abcde" "adflksj" "dslfi" "dfsl"])
  (simple-benchmark [] (clojure.string/join (transduce (map identity) conj els)) 10000)
;; 44
  (simple-benchmark [] (transduce (map identity) str els) 10000)
  ;; 37

  (def ctx {:context/elements [" sd" (elements/->BlockRef "x") "df "]
            :render/id "video"})
  (def c  (render-comp ctx))
  (def make-s (:assemble-url c))
  (make-s (fn [uid]
            (case uid "x" "content")))

  (def s "| some | text | here ")
  (def s "|")
  (def s "  |")
  (def s "abc |")
  (def s "|  ")
  (def s "")
  (def s "| d | ")
  (def s "b| | a")
  (def s " | | ")
  (clojure.string/split s #"\s*\|\s*")
  (clojure.string/split s #"\s*\|")
  (clojure.string/split s #"\|\s*")

  (let [[a & rest] [1 2 3]]
    (prn a rest))

  ;;;;;;;;;
  )
