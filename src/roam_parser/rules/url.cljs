(ns roam-parser.rules.url
  (:require
   [clojure.string]
   [roam-parser.transformations :as transf]
   [roam-parser.rules.relationships :refer [killed-by-of allowed-ctxs-fn]]
   [roam-parser.elements :as elements]
   [roam-parser.state :as state :refer [lookahead-contains? update-last-ctx remaining-str]]
   [roam-parser.utils :as utils]))

(def url-hint-re #"(?<![\w\.-])(?:ttps?://[a-zA-Z\d-\.]+\.[a-zA-Z\d-]{2,}|ww\.[a-zA-Z\d-\.]+\.[a-zA-Z\d-]{2,})")

(defn insert-url [state char]
  (let [ctx (-> state ::state/path peek)]
    (assoc state :roam-parser.state/path
           (-> (::state/path state)
               pop
               (transf/add-element (elements/->Url (subs (:string state) (:context/open-idx ctx) (:idx state)))
                                   state
                                   (:context/start-idx ctx)
                                   (:idx state))))))

(defn terminate-url [state char]
  (when (clojure.string/blank? char)
    (fn [state _] (transf/fallback-from-last state))))

(defn start-url [state char]
  (when (or (identical? char \h) (identical? char \w))
    (when-some [url-start (re-find url-hint-re (remaining-str state))]
      (transf/try-new-ctx {:context/id        :context.id/url
                           :context/open-idx  (-> state :idx)
                           :terminate-fallback insert-url
                           :context/terminate terminate-url}
                          state))))

(comment
  (simple-benchmark [] (re-find url-hint-re "this is some block that containes some ocnejkntf sh sd jfllk haklsolsolso solo wolololo egg butter") 100000)
  ;; 7

  (re-find #"d" "v")

  (simple-benchmark [] (clojure.string/blank? "g") 100000)
  ;; 11

  ;;;;;;;;;;
  )
