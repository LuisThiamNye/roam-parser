(ns roam-parser.transformations
  (:require
   [clojure.spec.alpha :as s]
   [roam-parser.rules.relationships :refer [allowed-ctxs-fn]]
   [roam-parser.context :as context]
   [roam-parser.utils :as utils]
   [roam-parser.state :as state :refer [get-sub]]
   [taoensso.timbre :as t]))

(defn process-char-partially [state state-actions]
  {:pre (s/valid? ::state/state state)}
  (let [char (nth (:string state) (:idx state))]
    (loop [i (dec (count state-actions))]
      (when (>= i 0)
        ;; TODO what is the point of returning a function when it can just simply return the transformed state?
        (let [action-fn (nth state-actions i)]
          (if-some [transform-state (action-fn state char)]
            (transform-state state #(subvec state-actions 0 i))
            (recur (dec i))))))))

(defn process-char [state state-actions]
  (or (process-char-partially state state-actions)
      (update state :idx inc)))

(defn fallback-from-open [ctx]
  (process-char (:prior-state ctx) (:fallback-rules ctx)))

(defn fallback-from-last [state]
  (t/debug "FALLBACK FROM END OF RUN")
  (let [ctx (-> state ::state/path peek)]
    (or (when-some [terminate-fallback (:terminate-fallback ctx)]
          (terminate-fallback state))
        (fallback-from-open ctx))))


(defn parent-killed-by? [ctx killer-ctx]
  (contains? (:context/killed-by ctx) (:context/id killer-ctx)))

;; TODO use regex escape var in rules/
(def str-replace-re (js/RegExp. (str "\\\\(?<escape>" (utils/re-to-str #"[\\{}\[\]\(\)`\*_\^:#!\n>]|\${2}") ")")
                                "g"))
(defn escape-str [^string string] (.replace string str-replace-re "$<escape>"))

(defn conj-text-el [els string ctx end-idx]
  (if (:context/ignore-text? ctx)
    els
    (let [text (get-sub string
                        (or (:context/last-idx ctx) (:context/open-idx ctx))
                        end-idx)]
      (cond-> els
        (not (identical? "" text)) (conj text)))))


(defn add-element [path el state el-start-idx next-idx]
  (let [ctx-n   (dec (count path))
        ctx     (peek path)
        ctx-els (:context/elements ctx)
        new-els (-> ctx-els
                    (conj-text-el (:string state) ctx el-start-idx)
                    (conj el))]
    (t/debug "FORM ELEMENT" new-els)
    (assoc path ctx-n
           (-> ctx
               (assoc :context/elements new-els)
               (assoc :context/last-idx next-idx)))))

(defn set-ctx-fallback [ctx state fallbacks]
  (-> ctx
      (assoc :fallback-rules fallbacks)
      (assoc :prior-state state)))

(defn start-new-ctx [ctx parent]
  {:pre (s/valid? ::context/ctx parent)}
  (fn [state get-fallbacks]
    (let [new-ctx (-> ctx
                      (assoc  :context/rules
                              (let [rules (-> state ::state/path peek :context/rules)]
                                (if-some [extra-rules (:context/extra-rules ctx)]
                                  (into rules extra-rules)
                                  (conj rules (:context/terminate ctx)))))
                      (cond-> (nil? (:context/allows-ctx? ctx))
                        (assoc :context/allows-ctx?
                               (allowed-ctxs-fn (:context/id ctx) (:context/allows-ctx? parent))))
                      (assoc :context/start-idx (:idx state))
                      (set-ctx-fallback state (get-fallbacks)))]
      (t/debug "START NEW CTX\n" new-ctx)
      (-> state
          (update ::state/path conj new-ctx)
          (assoc :idx (:context/open-idx ctx))))))

(defn try-new-ctx [ctx state]
  (let [parent (-> state ::state/path peek)]
    (when (or ((:context/allows-ctx? parent) (:context/id ctx))
              (contains? (:context/killed-by parent) (:context/id ctx)))
      (start-new-ctx ctx parent))))

(defn matches-ctx? [ctx id]
  (= id (:context/id ctx)))



(defn try-match-ctx [state-fn path closer-ctx-id killed-by]
  (let [last-index (dec (count path))]
    (loop [i last-index]
      (when-not (neg? i)
        (let [this-ctx (nth path i)]
          (if (matches-ctx? this-ctx closer-ctx-id)
            (if (= i last-index)
              ;; add the element
              (fn [state get-fallback]
                (state-fn
                 (update-in state [::state/path (dec (count path)) :context/elements]
                            (fn [els]
                              (conj-text-el els (:string state) this-ctx (:idx state))))
                 get-fallback))
              ;; in-between
              (fn [state _]
                (t/debug "OVERLAP BACKTRACK - unclosed ctx in the way of termination. Falling back from\n" (peek path))
                (fallback-from-last  state)))
            ;; unclosed ctx in-between that kills the target ctx, so abort to wait for killer ctx to complete
            (when-not (contains? killed-by (:context/id this-ctx))
              (recur (dec i)))))))))

(defn ctx-to-element [path make-el closer-data]
  (-> (fn [state _]
        (let [parent-ctx (-> state ::state/path pop peek)
              ctx        (-> state ::state/path peek)
              next-idx   (:next-idx closer-data)]
          (if (parent-killed-by? parent-ctx ctx)
            (do (t/debug "CTX KILLED by parent when CLOSING - parent, child:\n" parent-ctx ctx)
                (fallback-from-open parent-ctx))
            (if-some [new-el (make-el ctx)]
              (-> state
                  (assoc ::state/path (-> (:roam-parser.state/path state)
                                          pop
                                          (add-element new-el
                                                       state (:context/start-idx ctx) next-idx)))
                  (assoc :idx next-idx))
              (do
                (t/debug "CTX->EL FAILED")
                (fallback-from-open ctx))))))
      (try-match-ctx path (:context/id closer-data) (:killed-by closer-data))))


(defn replace-last [coll new]
  (assoc coll (-> coll count dec) new))

(defn clear-ctx
  [closer-length]
  (fn [state _]
    (t/debug "CLEAR CTX" (-> state ::state/path peek))
    (-> state
        (update ::state/path pop)
        (update :idx + closer-length))))

(defn new-single-element [el start-idx next-idx]
  (fn [state _]
    (-> state
        (update ::state/path add-element el state start-idx next-idx)
        (assoc :idx next-idx))))

(defn swap-ctx [path new-ctx token-data]
  (try-match-ctx
   (fn [state get-fallbacks]
     (let [old-ctx (-> state ::state/path peek)]
       (t/debug "SWAP ctx" old-ctx "\nto (partial)" new-ctx)
       (-> state
           (update ::state/path (fn [path]
                                             (assoc path (dec (count path))
                                                    (-> new-ctx
                                                        (assoc  :context/rules
                                                                (let [rules (-> old-ctx :context/rules)]
                                                                  (if-some [extra-rules (:context/extra-rules new-ctx)]
                                                                    (-> rules pop (into extra-rules))
                                                                    (assoc rules (dec (count rules))
                                                                           (:context/terminate new-ctx)))))
                                                        (assoc :prior-state state)
                                                        (cond-> (nil? (:context/allows-ctx? new-ctx))
                                                          (assoc :context/allows-ctx?
                                                                 (allowed-ctxs-fn (:context/id new-ctx)
                                                                                  (:context/allows-ctx? (-> state ::state/path peek peek)))))
                                                        (assoc :context/start-idx (:context/start-idx old-ctx))
                                                        (assoc :context/fallback-rules (get-fallbacks))
                                                        (assoc :context/replaced-ctx old-ctx)))))
           (assoc :idx (:context/open-idx new-ctx)))))
   path (:context/id token-data) (:killed-by token-data)))

(comment
  (def x [1 2 3 4 5 6 78 95 {:somethig 4} {4 4}])

  (simple-benchmark [] (-> x pop pop (conj {:a :b})) 10000)
;; 13
  (simple-benchmark [] (-> x pop (assoc (dec (count x)) {:a :b})) 10000)
  ;; 10

  (def path [{:c/a [4]}])
  (-> path peek :c/a)
  (require 'clojure.string)
  (simple-benchmark [] (= "" "") 100000)
;; 9
  (simple-benchmark [] (identical? "" "") 100000)
;; 0

  (simple-benchmark [] (when-let [x nil] true) 100000)
;; 2
  (simple-benchmark [] (when-some [x nil] true) 100000)
;; 1
  (simple-benchmark [] (when (fn? nil) true) 100000)
  ;; 7

  (defn f1 [x] "good")
  (defn f2 ([x] "good") ([x y] "good"))
  (simple-benchmark [] (f1 4) 10000)
  ;; 1
  (simple-benchmark [] (f2 4) 10000)
  ;; 3

;; =======
  ;;
  )
