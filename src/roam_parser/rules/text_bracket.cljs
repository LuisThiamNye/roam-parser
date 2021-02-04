(ns roam-parser.rules.text-bracket
  (:require
   [roam-parser.transformations :as transf]
   [roam-parser.state :refer [update-last-ctx]]
   [taoensso.timbre :as t]))

(defn terminate-text-bracket-fn [close-char n]
  (fn [_ char]
    (when (identical? close-char char)
      (t/debug "CLOSE simple" char)
      (fn [state get-fallbacks]
        (-> state
            (update :idx inc)
            (update-last-ctx (fn [ctx]
                               (-> ctx (assoc :terminate-fallback
                                              (fn [_] (transf/process-char state (get-fallbacks))))
                                   (assoc-in [:context/rules n] (constantly nil))))))))))

(defn start-text-bracket-fn [open-char close-char]
  (fn [_ char]
    (when (identical? open-char char)
      (fn [state _]
        (t/debug "START simple" char)
        (-> state
            (update :idx inc)
            (update-last-ctx (fn [ctx]
                               (-> ctx
                                   (update :context/rules
                                           #(conj %  (terminate-text-bracket-fn  close-char (count %))))))))))))
