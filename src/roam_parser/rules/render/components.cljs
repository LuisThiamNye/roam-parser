(ns roam-parser.rules.render.components
  (:require
   [clojure.string]
   [clojure.spec.alpha :as s]
   [taoensso.timbre :as t]
   [roam-parser.utils :as utils]
   [roam-parser.rules.relationships :refer [killed-by-of block-ctxs]]
   [roam-parser.elements :as elements]
   [roam-parser.rules.render.sub-ctxs :refer [start-curly-list start-string CurlyList]]))

;; (defprotocol ComponentProtocol
;;   (render-ctx-data [_]))

;; (defrecord TextArgComp [id]
;;   )


(defn delta-id? [id]
  (= 8710 (.charCodeAt id 0)))

(defn render-ctx-data [render-id]
  (case render-id
    ("calc"
     "youtube"
     "video"
     "iframe"
     "pdf") [#{:context.id/block-ref} false]
    ("embed"
     "mentions") [#{:context.id/block-ref :context.id/page-link} true]
    "attr-table" [#{:context.id/page-link} true]
    "roam/render" [#{:context.id/block-ref} true]
    "=" [block-ctxs false]
    "or" [block-ctxs false]
    "query" [#{:context.id/curly-list} false [start-curly-list]]
    (if (delta-id? render-id)
      [#{:context.id/curly-list} false [start-string start-curly-list]]
      [#{:context.id/block-ref :context.id/page-link}
       false])))

(defn elements-empty? [els]
  (or (zero? (count els))
      (and (string? (peek els))
           (= 1 (count els))
           (clojure.string/blank? (peek els)))))

(def delta-shorthand-re #"^\s*(\d+(?:\.\d+)?)(?: *([-\+\*]) *(\d+(?:\.\d+)?))?\s*$")

(def default-delta-comp #:delta{:delay 1
                                :operation (constantly 0)
                                :modifier 0})

(defn parse-delta-shorthand [input]
  (when-some [[_ delay-str operation-str modifier-str] (re-find delta-shorthand-re input)]
    #:delta{:delay (utils/str->float delay-str)
            :operation (case operation-str
                         "-" -
                         "+" +
                         "/" /
                         "*" *)
            :modifier (utils/str->float modifier-str)}))




(defn trim-elements-start [els]
  (if (zero? (count els))
els
    (let [first-el (nth els 0)
                          trimmed-str (cond-> first-el (string? first-el)
                                              (clojure.string/replace #"^\s*" ""))]
                      (if (identical? "" trimmed-str)
                        (subvec els 1)
                        (assoc els 0 trimmed-str)))))

(defn trim-elements-end [els]
  (if (zero? (count els))
    els
    (let [last-el (peek els)
         trimmed-str (cond-> last-el (string? last-el)
                             (clojure.string/replace #"\s*$" ""))]
     (if (identical? "" trimmed-str)
       (pop els)
       (utils/assoc-last els trimmed-str)))))

(defn trim-elements [els]
  (-> els
      trim-elements-start
      trim-elements-end))

(defn clean-groups [groups]
  (-> groups
      ;; trim off empty group at the end
      (as-> gs (cond-> gs (-> gs peek count zero?) pop))
                               ;; remove leading whitespace
      (update 0 trim-elements-start)
                               ;; remove trailing whitespace
      (utils/update-last trim-elements-end)))

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

(declare parse-query)

(defn query-condition? [x] (map? x))

(defn parse-query-condition [els operator]
  {:query/operator operator
   :operands (transduce (comp
                         (map (fn [x]
                                (if (instance? CurlyList x)
                                  (parse-query x)
                                  x)))
                         (filter #(or (instance? elements/PageLink %)
                                      (query-condition? %)
                                      (instance? elements/BlockRef %))))
                        conj (rest els))})

(defn parse-query [q-list]
  (let [els (:children q-list)]
    (case (first els)
      "and:" (parse-query-condition els :query/and)
      "or:" (parse-query-condition els :query/or)
      "not:" (parse-query-condition els :query/not)
      "between:" (parse-query-condition els :query/between)
      {:error (str "Error processing " (elements/stringify q-list)
                   "\nExpected one of:"
                   "\n{and:"
                   "\n{or:"
                   "\n{not:"
                   "\n{between:")})))

(defn component-query [ctx]
  (let [[first-el second-el :as els] (trim-elements (:context/elements ctx))]
    (cond
      (> (count els) 2) nil
      (and (string? first-el) (instance? CurlyList second-el))
      {:title (clojure.string/trim first-el)
       :query (parse-query second-el)}
      (and (instance? CurlyList first-el) (nil? second-el))
      {:query (parse-query first-el)})))

(defn single-ref-comp [ctx arg-types]
  (let [els (-> ctx :context/elements trim-elements)
        el  (peek els)]
    (when (= 1 (count els))
      (if (some #(instance? % el) arg-types)
        {:element el}
        (t/warn "unrecognised element in" (:render/id ctx) "of type" (type el))))))



(defn text-arg-comp [ctx]
  (let [els (:context/elements ctx)]
    (when-not (elements-empty? els)
      {:substitute-refs
       (fn [get-block-string]
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
           str els)))})))

(defn =-comp [ctx]
  (let [els (:context/elements ctx)
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
     :hidden-elements hidden}))

(defn or-comp [ctx]
  (let [els (:context/elements ctx)
        groups (-> (reduce (fn [groups el]
                             (split-el-into-groups groups el :no-limit))
                           [[]]
                           els)
                   clean-groups)]
    {:groups groups}))

(defn delta-comp [ctx]
  (let [els (:context/elements ctx)
        first-el (first els)]
    (if (elements-empty? els)
      default-delta-comp
      (when (string? first-el)
        (parse-delta-shorthand first-el)))))

(defn render-comp [ctx]
  (let [render-id (:render/id ctx)]
    (case render-id
      ("youtube"
       "video"
       "iframe"
       "pdf"
       "calc") (text-arg-comp ctx)
      ("embed"
       "mentions") (single-ref-comp ctx [elements/PageLink elements/BlockRef])
      "attr-table" (single-ref-comp ctx [elements/PageLink])
      "roam/render" (single-ref-comp ctx [elements/BlockRef])
      "=" (=-comp ctx)
      "or" (or-comp ctx)
      "query" (component-query ctx)
      (if (delta-id? render-id)
        (delta-comp ctx)
        :simple))))

(defn empty-comp [id]
  (when (delta-id? id)
    default-delta-comp))


(defn configure-render-ctx [ctx]
  (let [id (:render/id ctx)
        [allowed-ctxs ignore-text? extra-rules]
        (render-ctx-data id)]
    (-> ctx
        (assoc :context/ignore-text? ignore-text?)
        (assoc :context/allows-ctx? #(contains? allowed-ctxs %))
        (assoc :context/extra-rules (or extra-rules [])))))
