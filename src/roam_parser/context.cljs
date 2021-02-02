(ns roam-parser.context)

(defrecord Context [])

(defprotocol ContextProtocol
  (fallback [ctx]))

(defn create-ctx [fields]
  (extend-type (map->Context fields)))
