(ns cljsjs.highlight
  (:require ["highlight.js" :as hljs]))

(js/goog.exportSymbol "hljs" hljs)
(js/goog.exportSymbol "DevcardsSyntaxHighlighter" hljs)
