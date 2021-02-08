(ns roam-parser.core
  (:require
   [clojure.string]
   [roam-parser.utils :as utils]
   [roam-parser.builder :as builder]
   [roam-parser.elements :as el]))

(defn parse-block [^string string]
  (builder/find-elements string))

(defn stringify [element] el/stringify)
