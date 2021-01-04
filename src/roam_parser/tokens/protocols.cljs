(ns roam-parser.tokens.protocols)

(defprotocol TokenSequenceProtocol
  (process-tokens [_ parser-parameters parser-state]))

(defprotocol TokenProtocol
  (group-type [_]))
