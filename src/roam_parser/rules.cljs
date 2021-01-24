(ns roam-parser.rules
  (:require
   [clojure.string]
   [roam-parser.transformations :as transf]
   [roam-parser.elements :as elements]
   [roam-parser.utils :as utils]))


(defn killed-by-of [ctx-id]
  (case ctx-id
    :context.id/page-link #{:context.id/image-square
                            :context.id/image-round}
    :context.id/bracket-tag #{:context.id/image-square
                              :context.id/image-round}
    :context.id/alias-square #{}
    :context.id/alias-round #{}
    :context.id/image-square #{}
    :context.id/image-round #{}
    :context.id/parenthetical #{}
    :context.id/block-ref #{}
    :context.id/render #{}
    :context.id/attribute #{:context.id/codeblock}
    :context.id/formatting #{:context.id/codeblock
                             :context.id/image-square
                             :context.id/render}
    :context.id/codeblock #{}
    :context.id/code #{:context.id/codeblock}
    :context.id/block-quote #{}
    :context.id/latex #{}
    ))

(defn allowed-ctxs [ctx-id]
  (case ctx-id
    :context.id/block #{:context.id/code
                        :context.id/codeblock
                        :context.id/attribute
                        :context.id/render
                        :context.id/url
                        :context.id/formatting
                        :context.id/latex
                        :context.id/alias-square
                        :context.id/image-square
                        :context.id/parenthetical
                        :context.id/page-link
                        :context.id/bracket-tag
                        :context.id/tag
                        :context.id/block-ref
                        :context.id/hr}
    :context.id/page-link #{:context.id/page-link
                            :context.id/formatting
                            :context.id/alias-square}
    :context.id/bracket-tag #{:context.id/page-link
                              :context.id/formatting
                              :context.id/alias-square}
    :context.id/alias-square #{:context.id/latex
                               :context.id/formatting
                               :context.id/code
                               :context.id/image-square}
    :context.id/alias-round #{:context.id/block-ref
                              :context.id/page-link}
    :context.id/image-square #{:context.id/formatting
                               :context.id/image-square}
    :context.id/image-round #{}
    :context.id/parenthetical #{:context.id/code
                                :context.id/render
                                :context.id/url
                                :context.id/formatting
                                :context.id/latex
                                :context.id/alias-square
                                :context.id/image-square
                                :context.id/parenthetical
                                :context.id/page-link
                                :context.id/bracket-tag
                                :context.id/tag
                                :context.id/block-ref}
    :context.id/block-ref #{}
    :context.id/attribute #{:context.id/codeblock}
    :context.id/formatting #{:context.id/codeblock
                             :context.id/image-square
                             :context.id/render}
    :context.id/codeblock #{}
    :context.id/code #{:context.id/codeblock}
    :context.id/block-quote #{:context.id/code
                              :context.id/render
                              :context.id/url
                              :context.id/formatting
                              :context.id/latex
                              :context.id/alias-square
                              :context.id/image-square
                              :context.id/parenthetical
                              :context.id/page-link
                              :context.id/bracket-tag
                              :context.id/tag
                              :context.id/block-ref
                              :context.id/hr}
    :context.id/latex #{}))

(def escapable-char-regex #"[\\{}\[\]\(\)`\*_\^:#!\n>]|\${2}")
(def codeblock-re (js/RegExp #"^(?:(javascript|css|html|clojure|common lisp)\n|\n?)(.*(?:[^\n]|$))" "s"))
(def default-codeblock-lang "javascript")

;; TODO
(defn lookahead-contains? [state s]
  (clojure.string/starts-with? (subs (:string state) (inc (:idx state))) s))


(defn skip-escape-char [state char]
  (when (identical? \\ char)
    #(update % :idx + 2)))

;; Page link
;;



(defn terminate-page-link [state char]
  (when (identical? "]" char)
    (when (lookahead-contains? state "]")
      (let [idx     (:idx state)]
        (transf/ctx-to-element (:path state)
                               (fn [ctx]
                                 (elements/->PageLink
                                  (subs (:string state) (:context/open-idx ctx) idx)
                                  (:context/elements ctx)))
                               {:context/id :context.id/page-link
                                :killed-by? (killed-by-of :context.id/page-link) ;; TODO use polymorphism? implement killed-by?
                                :next-idx   (+ 2 idx)})))))

(defn start-page-link [state char]
  (when (identical? "[" char)
    (let [double? (lookahead-contains? state "[")]
      (when double?
        (transf/new-ctx {:context/id        :context.id/page-link
                         :context/open-idx  (-> state :idx (+ 2))
                         :context/elements  []
                         :context/killed-by (killed-by-of :context.id/page-link)
                         :context/allowed-ctxs (allowed-ctxs :context.id/page-link)
                         :context/terminate terminate-page-link}
                        state)))))

(defn block-ref? [el]
  ;; TODO
  )

(defn string-contents [ctx state]
  (transf/get-sub (:string state) (:context/open-idx ctx) (:idx state)))

;; Alias
;;

;; TODO alias needs a handle to the matching ctx
(defn terminate-alias-round-fn [round-id]
  (fn [state char]
    (when (identical? ")" char)
      (transf/ctx-to-element (:path state)
                             (case round-id
                               :context.id/alias-round
                               (fn [ctx]
                                 (let [[dest-type
                                        dest] (or (let [dest-els    (:context/elements ctx)
                                                        first-child (first dest-els)]
                                                    (cond
                                                      (block-ref? first-child)
                                                      [:block-ref "TODO"]

                                                      (instance? elements/PageLink first-child)
                                                      [:page (:page-name first-child)]))

                                                  [:url (string-contents ctx state)])]
                                   (elements/->Alias (:context/elements (:context/replaced-ctx ctx)) dest-type dest)))

                               :context.id/image-round
                               (fn [ctx]
                                 (elements/->Image (-> ctx :context/replaced-ctx :context/elements)
                                                   :url
                                                   (string-contents ctx state))))
                             {:context/id round-id
                              :killed-by  (killed-by-of round-id)
                              :next-idx   (-> state :idx inc)}))))

(defn terminate-alias-square-fn [square-id round-id]
  (fn [state char]
    (when (and (identical? "]" char)
               (lookahead-contains? state "("))
      (transf/swap-ctx (:path state)
                       {:context/id round-id
                        :context/open-idx  (-> state :idx (+ 2))
                        :context/elements  []
                        :context/killed-by (killed-by-of round-id)
                        :context/allowed-ctxs (allowed-ctxs round-id)
                        :context/terminate (terminate-alias-round-fn round-id)}
                       {:context/id square-id
                        :killed-by (killed-by-of square-id)}))))

(defn start-alias-square [state char]
  (when (identical? "[" char)
    (transf/new-ctx {:context/id        :context.id/alias-square
                     :context/open-idx  (-> state :idx inc)
                     :context/elements  []
                     :context/killed-by (killed-by-of :context.id/alias-square)
                     :context/allowed-ctxs (allowed-ctxs :context.id/alias-square)
                     :context/terminate (terminate-alias-square-fn :context.id/alias-square :context.id/alias-round)} state)))

(defn start-image-square [state char]
  (when (and (identical? \! char)
             (lookahead-contains? state "["))
    (transf/new-ctx {:context/id :context.id/image-square
                     :context/open-idx (-> state :idx (+ 2))
                     :context/elements []
                     :context/killed-by (killed-by-of :context.id/image-square)
                     :context/allowed-ctxs (allowed-ctxs :context.id/image-square)
                     :context/terminate (terminate-alias-square-fn :context.id/image-square :context.id/image-round)}
                    state)))

;; processed from end to beginning
(def rules [skip-escape-char start-page-link start-image-square start-alias-square])

;;
;; comment
;;

(comment

  (def ex-s "the somethign that happened the other day is reallly troubling me and my \\$$faamilay ow aiohofpdd")
  (def re (js/RegExp. escapable-char-regex))
  (simple-benchmark [] (.exec re ex-s) (* 1000 7))
  ;; 4
  (simple-benchmark [] (clojure.string/join (seq ex-s)) (* 1000 7))
  ;; 88
  (simple-benchmark [] (loop [i (dec (count ex-s))]
                         (when-not (neg? i)
                           (nth ex-s i)
                           (recur (dec i)))) (* 1000 7))
  ;; 37
  ;; conclusion: regex is really fast

  ;;
  (simple-benchmark [] (name :context.id/some-id) 10000)
  ;; 3
  (simple-benchmark [x {:get :some-id}] (:get x) 10000)
  ;; 6
  (simple-benchmark [x :b] (case x :a 8 :b 7 :c 4 :d 2) 10000)
  ;; 3
  (simple-benchmark [x :context.id/some-id] (case (name x)
                                              "somdkf" 4
                                              "dsdfsd" 7
                                              "some-id" 8
                                              "else" 6) 10000)
  ;; 3
  (simple-benchmark [] (contains? nil :this-that) 10000)
;; 3
  (simple-benchmark [] (contains? #{} :this-that) 10000)
;; 3
  )
