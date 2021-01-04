(ns roam-parser.tokens.token (:require [roam-parser.tokens.protocols :refer [TokenProtocol]]))



(defrecord Blockquote [idx link-type]
  TokenProtocol
  (group-type [_] Blockquote))

(defrecord Codeblock [idx length direction]
  TokenProtocol
  (group-type [_] Codeblock))

(defrecord Backtick [idx length]
  TokenProtocol
  (group-type [_] Backtick))



(defrecord Curly [idx length direction]
  TokenProtocol
  (group-type [_] Curly))

(defrecord Square [idx length direction]
  TokenProtocol
  (group-type [_] Square))

(defrecord Round [idx length direction]
  TokenProtocol
  (group-type [_] Round))

(defrecord Latex [idx direction]
  TokenProtocol
  (group-type [_] Latex))

(defrecord Highlight [idx length direction]
  TokenProtocol
  (group-type [_] Highlight))

(defrecord Bold [idx length direction]
  TokenProtocol
  (group-type [_] Bold))

(defrecord Italic [idx length direction]
  TokenProtocol
  (group-type [_] Italic))

(defrecord Hr [idx]
  TokenProtocol
  (group-type [_] Hr))



(defrecord Url [idx length]
  TokenProtocol
  (group-type [_] Url))

(defrecord Attribute [idx length]
  TokenProtocol
  (group-type [_] Attribute))

(defrecord Tag [idx length page-name]
  TokenProtocol
  (group-type [_] Tag))

(defrecord Hash [idx]
  TokenProtocol
  (group-type [_] ::hash))

(defrecord Bang [idx]
  TokenProtocol
  (group-type [_] ::bang))
