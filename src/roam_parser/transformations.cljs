(ns roam-parser.transformations
  (:require
   [roam-parser.utils :as utils]
   [taoensso.timbre :as t]))

(defn process-char [state state-actions]
  (let [char (nth (:string state) (:idx state))]
    (loop [i (dec (count state-actions))]
      (if (neg? i)
        (update state :idx inc)
        ;; TODO what is the point of returning a function when it can just simply return the transformed state?
        (let [action-fn (nth state-actions i)]
          (if-some [transform-state (action-fn state char)]
            (transform-state state #(subvec state-actions 0 i))
            (recur (dec i))))))))

;; TODO
(defn state-fallback [state]
  (process-char (:last-state state) (:fallback-rules state)))

(defn fallback-from-ctx [ctx]
  (process-char (:state ctx) (:fallback-rules ctx)))

(defn parent-killed-by? [ctx killer-ctx]
  (contains? (:context/killed-by ctx) (:context/id killer-ctx)))

;; TODO use regex escape var in rules/
(def str-replace-re (js/RegExp. (str "\\\\(?<escape>" (utils/re-to-str #"[\\{}\[\]\(\)`\*_\^:#!\n>]|\${2}") ")")
                                "g"))
(defn escape-str [^string string] (.replace string str-replace-re "$<escape>"))

(defn get-sub
  ([^string string start end no-escape]
   (subs string start end))
  ([^string string start end]
   (-> (subs string start end)
       escape-str)))

(defn conj-text-el [els string ctx end-idx]
  (conj els (get-sub string
                     (or (:context/last-idx ctx) (:context/open-idx ctx))
                     end-idx)))

(defn add-element [path el state el-start-idx next-idx]
  (let [ctx-n   (dec (count path))
        ctx     (peek path)
        ctx-els (:context/elements ctx)
        new-el (-> ctx-els
                   (cond->
                    (not (:context/exclude-text? ctx))
                     (conj-text-el (:string state) ctx el-start-idx))
                   (conj el))]
    (t/debug "FORM ELEMENT" new-el)
    (assoc path ctx-n
           (-> ctx
               (assoc :context/elements new-el)
               (assoc :context/last-idx next-idx)))))

(defn start-new-ctx [ctx]
  (fn [state get-fallbacks]
    (let [new-ctx (-> ctx
                      (assoc  :context/rules
                              (conj (-> state :path peek :context/rules)
                                    (:context/terminate ctx)))
                      (assoc :context/start-idx (:idx state))
                      (assoc :state state)
                      (assoc :fallback-rules (get-fallbacks)))]
      (t/debug "START NEW CTX\n" new-ctx)
      (-> state
          (update :path conj new-ctx)
          (assoc :last-state state)
          (assoc :fallback-rules (get-fallbacks))
          (assoc :idx (:context/open-idx ctx))))))

(defn try-new-ctx [ctx state]
  (when (or (contains? (-> state :path peek :context/allowed-ctxs) (:context/id ctx))
            (contains? (-> state :path peek :context/killed-by) (:context/id ctx)))
    (start-new-ctx ctx)))

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
              (if (:context/exclude-text? this-ctx)
                state-fn
                (fn [state get-fallback]
                  (state-fn
                   (update-in state [:path (dec (count path)) :context/elements]
                              (fn [els]
                                (conj-text-el els (:string state) this-ctx (:idx state))))
                   get-fallback)))
              ;; in-between
              (fn [_ _]
                (t/debug "OVERLAP BACKTRACK - unclosed ctx in the way of termination. Falling back from\n" this-ctx)
                (fallback-from-ctx this-ctx)))
            (when-not (contains? killed-by (:context/id this-ctx))
              (recur (dec i)))))))))

(defn ctx-to-element [path make-el closer-data]
  (-> (fn [state _]
        (let [parent-ctx (-> state :path pop peek)
              ctx        (-> state :path peek)
              next-idx   (:next-idx closer-data)]
          (if (parent-killed-by? parent-ctx ctx)
            (do (t/debug "CTX KILLED by parent when CLOSING - parent, child:\n" parent-ctx ctx)
                (fallback-from-ctx parent-ctx))
            (-> state
                (assoc :path (-> (:path state)
                                 pop
                                 (add-element (make-el ctx)
                                              state (:context/start-idx ctx) next-idx)))
                (assoc :idx next-idx)))))
      (try-match-ctx path (:context/id closer-data) (:killed-by closer-data))))

(defn replace-last [coll new]
  (assoc coll (-> coll count dec) new))

(defn clear-ctx
  [closer-length]
  (fn [state _]
    (t/debug "CLEAR CTX" (-> state :path peek))
    (-> state
        (update :path pop)
        (update :idx + closer-length))))

(defn new-single-element [el next-idx]
  (fn [state _]
    (-> state
        (update :path add-element el state (-> state peek :context/start-idx) next-idx)
        (assoc :idx next-idx))))

(defn swap-ctx [path new-ctx token-data]
  (try-match-ctx
   (fn [state get-fallbacks]
     (let [old-ctx (-> state :path peek)]
       (t/debug "SWAP ctx" old-ctx "\nto (partial)" new-ctx)
       (-> state
           (update :path (fn [path]
                           (assoc path (dec (count path))
                                  (-> new-ctx
                                      (assoc  :context/rules
                                              (let [rules (-> old-ctx :context/rules)]
                                                (assoc rules (dec (count rules))
                                                       (:context/terminate new-ctx))))
                                      (assoc :state state)
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
;;
  )
