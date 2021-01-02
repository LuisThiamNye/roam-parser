(ns roam-parser.tokens.token )

(defprotocol TokenProtocol
  (group-type [_]))

(defrecord Blockquote [idx link-type]
  TokenProtocol
  (group-type [_] BlockquoteSequence))

(defrecord Codeblock [idx length direction]
  TokenProtocol
  (group-type [_] CodeblockSequence))

(defrecord Backtick [idx length]
  TokenProtocol
  (group-type [_] BacktickSequence))



(defrecord Curly [idx length direction]
  TokenProtocol
  (group-type [_] CurlySequence))

(defrecord Square [idx length direction]
  TokenProtocol
  (group-type [_] SquareSequence))

(defrecord Round [idx length direction]
  TokenProtocol
  (group-type [_] RoundSequence))

(defrecord Latex [idx direction]
  TokenProtocol
  (group-type [_] LatexSequence))

(defrecord Highlight [idx length direction]
  TokenProtocol
  (group-type [_] HighlightSequence))

(defrecord Bold [idx length direction]
  TokenProtocol
  (group-type [_] BoldSequence))

(defrecord Italic [idx length direction]
  TokenProtocol
  (group-type [_] ItalicSequence))

(defrecord Hr [idx]
  TokenProtocol
  (group-type [_] HrSequence))



(defrecord Url [idx length]
  TokenProtocol
  (group-type [_] UrlSequence))

(defrecord Attribute [idx length]
  TokenProtocol
  (group-type [_] AttributeSequence))

(defrecord Tag [idx length]
  TokenProtocol
  (group-type [_] TagSequence))

(defrecord Hash [idx char-type]
  TokenProtocol
  (group-type [_] ::hash))

(defrecord Bang [idx char-type]
  TokenProtocol
  (group-type [_] ::bang))
