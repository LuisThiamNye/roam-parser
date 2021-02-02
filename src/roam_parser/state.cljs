(ns roam-parser.state
  (:require
   [clojure.string]
   [roam-parser.utils :as utils]))


(defn lookahead-contains? [state s]
  (clojure.string/starts-with? (subs (:string state) (inc (:idx state))) s))

(defn remaining-str [state]
  (subs (:string state) (inc (:idx state))))
(defn preceding-str [state]
  (subs (:string state) 0 (:idx state)))

(defn update-last-ctx [state f]
  (update state :path utils/update-last f))


(defn get-sub
  ([^string string start end]
   (subs string start end)))

(defn string-contents [ctx state]
  (get-sub (:string state) (:context/open-idx ctx) (:idx state)))
