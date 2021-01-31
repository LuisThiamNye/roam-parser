(ns roam-parser.dev.preload
  (:require
   [taoensso.timbre :as timbre]
   [roam-parser.dev.timbre-support :refer [prefix-output-fn
                                           console-appender]]))


(timbre/merge-config! {:output-fn prefix-output-fn
                       :appenders {:console (console-appender)}})
