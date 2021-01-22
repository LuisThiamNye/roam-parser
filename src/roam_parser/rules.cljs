(ns roam-parser.rules
  (:require
   [clojure.string]
   [roam-parser.transformations :as transf]
   [roam-parser.elements :as elements]
   [roam-parser.utils :as utils]))


(def escapable-char-regex #"[\\{}\[\]\(\)`\*_\^:#!\n>]|\${2}")
(def codeblock-re (js/RegExp #"^(?:(javascript|css|html|clojure|common lisp)\n|\n?)(.*(?:[^\n]|$))" "s"))
(def default-codeblock-lang "javascript")

;; TODO
(defn lookahead-contains? [state s]
  (clojure.string/starts-with? (subs (:string state) (inc (:idx state))) s))


;; Page link
;;

(defn terminate-page-link [state char]
  (when (identical? "]" char)
    (when (lookahead-contains? state "]")
      (let [partner (-> state :path peek)
            idx     (:idx state)]
        (transf/ctx-to-element (:path state)
                               (elements/->PageLink
                                (subs (:string state) (:context/open-idx partner) idx)
                                (:context/elements partner))
                               {:context/id :context/page-link
                                :killed-by? (constantly false) ;; TODO use polymorphism? implement killed-by?
                                :next-idx   (+ 2 idx)})))))

(defn start-page-link [state char]
  (when (identical? "[" char)
    (let [double? (lookahead-contains? state "[" )]
      (when double?
        (transf/new-ctx {:context/id        :context/page-link
                         :context/open-idx  (-> state :idx (+ 2))
                         :context/elements  []
                         :context/terminate terminate-page-link})))))

(defn block-ref? [el]
  ;; TODO
  )

(defn string-contents [el]
  ;; TODO
  )

;; Alias
;;

(defn terminate-alias-round [state char]
  (when (identical? ")" char)
    (let [ctx    (-> state :path peek)
          [dest-type
           dest] (or (when (= :context/alias-round (:context/id ctx))
                       (let [dest-els    (:context/elements ctx)
                             first-child (first dest-els)]
                         (cond
                           (block-ref? first-child)
                           [:block-ref "TODO"]

                           (instance? elements/PageLink first-child)
                           [:page (:page-name first-child)])))

                     [:url (string-contents ctx)])]
      (transf/ctx-to-element (:path state)
                             (elements/->Alias (:context/elements state) dest-type dest)
                             {:context/id :context/alias-round
                              :killed-by? (constantly false) ;; TODO
                              :next-idx   (-> state :idx inc)}))))

(defn terminate-alias-square [state char]
  (when (and (identical? "]" char)
             (lookahead-contains? state "(" ))
    (transf/swap-ctx (:path state)
                     {:context/id        :context/alias-round
                      :context/open-idx  (-> state :idx (+ 2))
                      :context/elements  []
                      :context/terminate terminate-alias-round}
                     {:context/id :context/alias-square
                      :killed-by? (constantly false) ;; TODO
                      })))

(defn start-alias-square [state char]
  (when (identical? "[" char)
    (let [alias-square {:context/id        :context/alias-square
                        :context/open-idx  (-> state :idx inc)
                        :context/elements  []
                        :context/terminate terminate-alias-square}]
      (transf/new-ctx alias-square))))

;; processed from end to beginning
(def rules [start-page-link start-alias-square])
