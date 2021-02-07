(ns roam-parser.state
  (:require
   [clojure.string]
   [clojure.spec.alpha :as s]
   [roam-parser.context :as context]
   [roam-parser.rules.relationships :refer [block-ctxs]]
   [roam-parser.utils :as utils]))

(s/def ::path (s/coll-of ::context/ctx))
(s/def ::state (s/keys :req [::path]))

(defn initial-state [string rules]
  {::path   [{:context/id          :context.id/block
              :context/open-idx            0
              :context/elements    []
              :context/allows-ctx? #(contains? block-ctxs %)
              :context/killed-by   #{}
              :context/rules       rules}]
   :idx    0
   :string string})

(defn lookahead-contains? [state s]
  (clojure.string/starts-with? (subs (:string state) (inc (:idx state))) s))

(defn remaining-str [state]
  (subs (:string state) (inc (:idx state))))
(defn preceding-str [state]
  (subs (:string state) 0 (:idx state)))

(defn update-last-ctx [state f]
  (update state :roam-parser.state/path utils/update-last f))


(defn get-sub
  ([^string string start end]
   (subs string start end)))

(defn string-contents [ctx state]
  (get-sub (:string state) (:context/open-idx ctx) (:idx state)))
