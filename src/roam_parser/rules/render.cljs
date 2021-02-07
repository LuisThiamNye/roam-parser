(ns roam-parser.rules.render
  (:require
   [clojure.string]
   [clojure.spec.alpha :as s]
   [taoensso.timbre :as t]
   [roam-parser.utils :as utils]
   [roam-parser.rules.relationships :refer [killed-by-of block-ctxs]]
   [roam-parser.rules.text-bracket :refer [start-text-bracket-fn]]
   [roam-parser.state :refer [lookahead-contains? get-sub]]
   [roam-parser.elements :as elements]
   [roam-parser.transformations :as transf]))

(defn start-curly-list [state char]
  (when (identical? "{" char)))

(defn start-pipe [state char]
  (when (identical? \| char)
    (transf/new-single-element \| (:idx state) (-> state :idx inc))))

(defn elements-empty? [els]
  (or (zero? (count els))
      (and (string? (peek els))
           (= 1 (count els))
           (clojure.string/blank? (peek els)))))

(def delta-shorthand-re #"^\s*(\d+(?:\.\d+)?)(?: *([-\+\*]) *(\d+(?:\.\d+)?))?\s*$")

(defn parse-delta-shorthand [input]
  (when-some [[_ delay-str operation-str modifier-str] (utils/probe (re-find delta-shorthand-re input))]
    #:delta{:delay (utils/str->float delay-str)
            :operation (case operation-str
                         "-" -
                         "+" +
                         "/" /
                         "*" *)
            :modifier (utils/str->float modifier-str)}))

(defn delta-id? [id]
  (= 8710 (.charCodeAt id 0)))

(defn clean-groups [groups]
  (-> groups
      ;; trim off empty group at the end
      (as-> gs (cond-> gs (-> gs peek count zero?) pop))
                               ;; remove leading whitespace
      (update-in [0 0] (fn [el]
                         (cond-> el (string? el)
                                 (clojure.string/replace #"^\s*" ""))))
                               ;; remove trailing whitespace
      (utils/update-last (fn [last-g]
                           (utils/update-last last-g
                                              (fn [last-el]
                                                (cond-> last-el (string? last-el)
                                                        (clojure.string/replace #"\s*$" ""))))))))

(defn split-el-into-groups [groups el max-splits]
  (if (string? el)
                                           (let [[group-tail & new-group-strs] (clojure.string/split el #"\s*\|\s*" max-splits)]
                                             (if (nil? group-tail)
                                            ;; entire string is the delimiter
                                               (conj groups [])
                                               (-> groups
                                                   (cond-> (not (clojure.string/blank? group-tail))
                                                     (utils/update-last #(conj % group-tail)))
                                                   (as-> g (reduce (fn [gs s]
                                                                     (if (clojure.string/blank? s)
                                                                       gs
                                                                       (conj gs (vector s))))
                                                                   g
                                                                   new-group-strs))
                                                   (cond-> (re-find #"\|\s*$" el)
                                                     (conj [])))))
                                           (utils/update-last groups #(conj % el))))

(defn render-comp [ctx]
  (let [render-id (:render/id ctx)]
    (case render-id
      ("youtube"
       "video"
       "iframe"
       "pdf"
       "calc")
      (let [els (:context/elements ctx)]
        (when-not (elements-empty? els)
          {:substitute-refs (fn [get-block-string]
                              (s/assert (s/fspec :args (s/cat :uid string?)
                                                 :ret string?)
                                        get-block-string)
                              (clojure.string/trim
                               (transduce
                                (map (fn [el]
                                       (cond
                                         (instance? elements/BlockRef el)
                                         (get-block-string (:block-uid el))

                                         (string? el) el
                                         :else        (do (t/warn "Parsing " (:render/id ctx) ": found a non-blockref/string element of type" (type el))
                                                          (str)))))
                                str els)))}))
      ("embed"
       "mentions")
      (let [els (-> ctx :context/elements)
            el  (peek els)]
        (when (= 1 (count els))
          (condp instance? el
            elements/PageLink {:node/title (:page-name el)}
            elements/BlockRef {:block/uid (:block-uid el)}
            (t/warn "unrecognised element in" (:render/id ctx) "of type" (type el)))))
      "attr-table"
      (let [els (-> ctx :context/elements)
            el  (peek els)]
        (when (= 1 (count els))
          (condp instance? el
            elements/PageLink {:node/title (:page-name el)}
            (t/warn "unrecognised element in" (:render/id ctx) "of type" (type el)))))
      "="     (let [els (:context/elements ctx)
                    [visible hidden] (-> (loop [groups [[]]
                                                els-left els]
                                           (if-some [el (first els-left)]
                                             (let [next-groups (split-el-into-groups groups el 2)]
                                               (if (> (count next-groups) 1)
                                                 (utils/update-last next-groups #(into % (next els-left)))
                                                 (recur next-groups (next els-left))))
                                             groups))
                                         clean-groups)]
                {:visible-elements visible
                 :hidden-elements hidden})
      "or"    (let [els (:context/elements ctx)
                    groups (-> (reduce (fn [groups el]
                                         (split-el-into-groups groups el :no-limit))
                                       [[]]
                                       els)
                               clean-groups)]
                {:groups groups})
      "query" nil
      (if (delta-id? render-id)
        (let [els (:context/elements ctx)]
          (when-not (elements-empty? els)
            (let [first-el (first els)]
              (when (string? first-el)
                (parse-delta-shorthand first-el)))))
        :simple))))

(defn terminate-render [state char]
  (when (and (identical? "}" char)
             (lookahead-contains? state "}"))
    (let [idx     (:idx state)]
      (transf/ctx-to-element (:roam-parser.state/path state)
                             (fn [ctx]
                               (when-some [component (render-comp ctx)]
                                 (elements/->Render
                                  (:render/id ctx)
                                  (:linked? ctx)
                                  (:context/elements ctx)
                                  component)))
                             {:context/id :context.id/render
                              :killed-by (killed-by-of :context.id/render)
                              :next-idx   (+ 2 idx)}))))

(defn configure-render-ctx [ctx]
  (let [id (:render/id ctx)
        [allowed-ctxs ignore-text? extra-rules]
        (case id
          "calc" [#{:context.id/block-ref} false]
          ("youtube"
           "video"
           "iframe"
           "pdf") [#{:context.id/block-ref} false]
          ("embed"
           "mentions") [#{:context.id/block-ref :context.id/page-link} true]
          "attr-table" [#{:context.id/page-link} true]
          "=" [block-ctxs false ]
          "or" [block-ctxs false ]
          "query" [#{:context.id/curly-list} true [start-curly-list]]
          (if (delta-id? id)
            [#{:context.id/curly-list} false [start-curly-list]]
            [#{:context.id/block-ref :context.id/page-link}
             false]))]
    (-> ctx
        (assoc :context/ignore-text? ignore-text?)
        (assoc :context/allows-ctx? #(contains? allowed-ctxs %))
        (assoc :context/extra-rules (conj (or extra-rules []) terminate-render)))))


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
                           (configure-render-ctx
                            {:context/id         :context.id/render
                             :render/id          render-id
                             :linked?            linked?
                             :context/open-idx   (-> state :idx inc)
                             :context/elements   []
                             :context/text-rules [(start-text-bracket-fn  "{" "}")]
                             :context/killed-by  (killed-by-of :context.id/render)})
                           {:context/id :context.id/render-id
                            :killed-by  (killed-by-of :context.id/render-id)}))

        (and (identical? char "}")
             (lookahead-contains? state "}"))
        (when-some [[linked? render-id] (render-id-data state)]
          (transf/ctx-to-element (:roam-parser.state/path state)
                                 (fn [_]
                                   (elements/->Render render-id linked? nil nil))
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
