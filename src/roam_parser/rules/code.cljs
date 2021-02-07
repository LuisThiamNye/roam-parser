(ns roam-parser.rules.code
  (:require
   [clojure.string]
   [roam-parser.rules.relationships :refer [killed-by-of]]
   [roam-parser.transformations :as transf]
   [roam-parser.elements :as elements]
   [roam-parser.state :refer [remaining-str preceding-str]]))

(defn valid-lang? [lang]
  (contains? #{"clojure" "javascript"
               "common lisp"}
             lang))

(defn terminate-codeblock [state char]
  (when (and (identical? \` char)
             (re-find #"(?m)^``$" (remaining-str state)))
    (transf/ctx-to-element (:roam-parser.state/path state)
                           (fn [ctx]
                             (let [content (or (some-> ctx :context/elements peek) "")]
                               (elements/->Codeblock (:codeblock/lang ctx)
                                                     (cond-> content
                                                       (clojure.string/ends-with? content "\n")
                                                       (subs 0 (dec (count content)))))))
                           {:context/id :context.id/codeblock
                            :killed-by  (killed-by-of :context.id/codeblock)
                            :next-idx   (-> state :idx (+ 4))})))

(defn start-codeblock [state char]
  (when (and (identical? \` char)
             (re-find #"(?m)^$" (preceding-str state)))
    (let [lang (some-> (re-find #"(?m)^``[\w ]*$" (remaining-str state))
                       (subs 2))]
      (when (valid-lang? lang)
        (transf/try-new-ctx {:context/id        :context.id/codeblock
                             :codeblock/lang lang
                             :context/open-idx  (-> state :idx (+ 4 (count lang)))
                             :context/elements  []
                             :context/killed-by (killed-by-of :context.id/codeblock)
                             :context/terminate terminate-codeblock}
                            state)))))

(defn excess-backticks [state]
  (re-find #"^`*" (remaining-str state)))

(defn terminate-code [state char]
  (when (and (identical? char \`)
             (not (identical? (-> state :roam-parser.state/path peek :context/open-idx) (:idx state))))
    (let [excess (excess-backticks state)
          excess-count (count excess)]
      (transf/ctx-to-element (:roam-parser.state/path state)
                             (fn [ctx]
                               (elements/->Code (-> ctx :context/elements peek
                                                    (cond-> (pos? excess-count)
                                                      (str excess)))))
                             {:context/id :context.id/code
                              :killed-by (killed-by-of :context.id/code)
                              :next-idx (-> state :idx (+ 1 excess-count))}))))

(defn start-code [state char]
  (when (identical? char \`)
    (let [excess       (excess-backticks state)
          excess-count (count excess)
          try-new-code (transf/try-new-ctx {:context/id        :context.id/code
                                            :context/open-idx  (-> state :idx inc)
                                            :context/elements  []
                                            :context/killed-by (killed-by-of :context.id/code)
                                            :context/terminate terminate-code}
                                           state)]
      (if (and (> excess-count 2) (some? try-new-code))
        (fn [state get-fallbacks]
          (-> state (try-new-code #(conj (get-fallbacks)
                                         (fn [state _]
                                           (transf/new-single-element (elements/->Code (subs excess 1))
                                                                      (-> state :idx (+ 1 excess-count))))))
              (update :idx + 1 excess-count)))
        try-new-code))))
