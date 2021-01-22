(ns roam-parser.transformations
  (:require
   [roam-parser.utils :as utils]))

(defn process-char [state state-actions]
  (let [char (nth (:string state) (:idx state))]
    (loop [i (dec (count state-actions))]
      (if (neg? i)
        (update state :idx inc)
        ;; TODO what is the point of returning a function when it can just simply return the transformed state?
        (if-some [transform-state ((nth state-actions i) state char) ]
          (transform-state state #(subvec state-actions 0 i))
          (recur (dec i)))))))

;; TODO
(defn fallback-state [ctx]
  (process-char (:state ctx) (:context/fallback-rules ctx)))


(defn parent-killed-by? [ctx killer-ctx]
  (contains? (:context/killed-by ctx) (:context/id killer-ctx)))

(defn add-element [path el]
  (update-in path [(dec (count path)) :context/elements]
             conj el))

(defn new-ctx [ctx]
  (fn [state get-fallbacks]
    (-> state
        (update :path conj
                (-> ctx
                    (assoc  :context/rules
                            (conj (-> state :path peek :context/rules)
                                  (:context/terminate ctx)))
                    (assoc :state state)
                    (assoc :context/fallback-rules (get-fallbacks))))
        (assoc :idx (:context/open-idx ctx)))))

(defn matches-ctx? [ctx id]
  (= id (:context/id ctx)))

(defn check-unclosed-children [state-fn path ctx-id killed-by?]
  (let [last-index (dec (count path))]
    (loop [i last-index]
      (when-not (neg? i)
        (let [this-ctx (nth path i)]
          (if (matches-ctx? this-ctx ctx-id)
            (if (= i last-index)
              ;; add the element
              state-fn
              ;; in-between
              (let [failed-context (nth path (inc i))]
                (fn [_ _]
                  (fallback-state failed-context))))
            (when-not (killed-by? this-ctx)
              (recur (dec i)))))))))

(defn ctx-to-element [path el closer-data]
  (-> (fn [state _]
        (let [parent-ctx (-> state :path pop peek)
              ctx        (-> state :path peek)]
          (if (parent-killed-by? parent-ctx ctx)
            (fallback-state parent-ctx)
            (-> state
                (assoc :path (-> (:path state)
                                 pop
                                 (add-element el)))
                (assoc :idx (:next-idx closer-data))))))
      (check-unclosed-children path (:context/id closer-data) (:killed-by? closer-data))))

(defn new-single-element [el next-idx]
  (fn [state _]
    (-> state
        (update :path add-element el)
        (assoc :idx next-idx))))
;; TODO find in-betweens
(defn swap-ctx [path new-ctx token-data]
  (check-unclosed-children
   (fn [state get-fallbacks]
     (-> state
         (update :path (fn [path]
                         (assoc path (dec (count path))
                                (-> new-ctx
                                    (assoc  :context/rules
                                            (let [rules (-> state :path peek :context/rules)]
                                              (assoc rules (dec (count rules))
                                                     (:context/terminate new-ctx))))
                                    (assoc :state state)
                                    (assoc :context/fallback-rules (get-fallbacks))))))
         (assoc :idx (:context/open-idx new-ctx))))
   path (:context/id token-data) (:killed-by? token-data)))
