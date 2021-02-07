(ns roam-parser.context
  (:require
   [clojure.spec.alpha :as s]))

(s/def :context/allows-ctx? fn?)
(s/def :context/elements vector?)
(s/def ::ctx (s/keys :req [:context/allows-ctx? :context/elements]))

(comment
  (defrecord Context [])

   (defprotocol ContextProtocol
     (fallback [ctx]))

   (defn create-ctx [fields]
     (extend-type (map->Context fields))))
