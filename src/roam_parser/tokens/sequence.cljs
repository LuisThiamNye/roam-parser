(ns roam-parser.tokens.sequence (:require [roam-parser.tokens.token :as token]
                                          [clojure.string]
                                          [roam-parser.tokens.protocols :refer [TokenSequenceProtocol process-tokens]]
                                          [roam-parser.elements :as el]
                                          [roam-parser.utils :as utils]
                                          [roam-parser.rules :as rules]
                                          [roam-parser.builder :as builder]))



(defn on-abandoned-close [current-results current-token]
  (case (:id current-token)
    :codeblock (conj current-results {:start (:idx current-token)
                                      :end (+ (:idx current-token) (:length current-token))
                                      :id :code
                                      :children-start (inc (:idx current-token))
                                      :children-end (dec (+ (:idx current-token) (:length current-token)))
                                      :content (apply str (repeat (- (:length current-token) 2) "`"))})
    current-results))

(defn process-paired [injections token-key parser-parameters parser-state]
  (let [close-delimiter  (:close-delimiter injections)]
    (loop [state (assoc parser-state :pending-series-tokens (get-in parser-state [:pending-tokens token-key])
                        :pending-tokens (dissoc (:pending-tokens parser-state) token-key)
                           ; :elements (get parser-state :elements)
                        :free-openers [])]
      (let [these-pending-tokens (get state :pending-series-tokens)]
        (if (pos? (count these-pending-tokens))
          (let [current-token (nth these-pending-tokens 0)
                {:keys [direction]} current-token
                next-state (update state :pending-series-tokens subvec 1)]
            (recur (if (= :open direction)
                     (update next-state :free-openers
                             conj current-token)
            ;; found potential closer
                     (if-let [partner (peek (:free-openers state))]
              ;; found a matching pair, add to results
                       (close-delimiter parser-parameters state current-token partner)
                ;; found a (potential) closer but no partner found
                       (if (= :close direction)
                       ;; close -> see if anything should be done with orphaned closer
                         (update next-state :elements
                                 on-abandoned-close current-token)
                       ;; dual -> add as opener
                         (update next-state :free-openers
                                 conj current-token))))))
        ;; no more tokens of this type to process -> return
          state)))))



(defn close-codeblock [m parser-parameters]
  (let [[_ lang content] (->> (builder/get-sub (:block-string parser-parameters) (:children-start m) (:children-end m) :no-escape)
                              (.exec rules/codeblock-re))]
    (assoc (el/map->Codeblock m)
           :language (if (nil? lang) rules/default-codeblock-lang lang)
           :content  (builder/escape-str content)
           :raw-content content)))

(defrecord CodeblockSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (as-> (process-paired {:close-delimiter (builder/close-ambiguous-fn
                                             3 close-codeblock)} token/Codeblock parser-parameters parser-state)
          state
      ;; turn orphaned runs of ```` into coded backticks
      (reduce (fn [s o]
                (update s :elements conj
                        (el/map->Codeblock {:start          (:idx o)
                          :end            (+ (:idx o) (:length o))
                          :children-start (inc (:idx o))
                                            :children-end   (dec (+ (:idx o) (:length o)))
                                            :language rules/default-codeblock-lang
                          :content        (apply str (repeat (- (:length o) 2) "`"))})))
              state (:free-openers state)))))

(defn close-backtick [m parser-parameters]
  (let [raw-content (builder/get-sub (:block-string parser-parameters)
                                     (:children-start m) (:children-end m) :no-escape)]
    (assoc (el/map->Code m)
           :content (builder/escape-str raw-content)
           :raw-content raw-content)))

(defrecord BacktickSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (process-paired {:close-delimiter (builder/close-ambiguous-fn
                                       1 close-backtick)}
                    token/Backtick parser-parameters parser-state)))

;; TODO ignore any of length 1 if curly not allowed
(defrecord CurlySequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (process-paired {:close-delimiter builder/close-curly} token/Curly parser-parameters parser-state)))



(defrecord SquareSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (process-paired {:close-delimiter builder/close-square} token/Square parser-parameters parser-state)))

(defrecord RoundSequence [tokens]
  TokenSequenceProtocol
 (process-tokens [this parser-parameters parser-state]
    (process-paired {:close-delimiter builder/close-round} token/Round parser-parameters parser-state)))


(defn close-latex [m parser-parameters]
  (assoc (el/map->Latex m) :content (builder/get-sub (:block-string parser-parameters) (:children-start m) (:children-end m))))

(defrecord LatexSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (process-paired {:close-delimiter (builder/close-ambiguous-fn 2 close-latex)} token/Latex parser-parameters parser-state)))

(defn close-formatting-fn [format-type]
  #(assoc (el/map->Formatting %) :format-type format-type))

(defrecord HighlightSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (process-paired {:close-delimiter (builder/close-ambiguous-fn 2 (close-formatting-fn :format-type/highlight))} token/Highlight parser-parameters parser-state)))

(defrecord BoldSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (process-paired {:close-delimiter (builder/close-ambiguous-fn 2 (close-formatting-fn :format-type/bold))} token/Bold parser-parameters parser-state)))

(defrecord ItalicSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (process-paired {:close-delimiter (builder/close-ambiguous-fn 2 (close-formatting-fn :format-type/italic))} token/Italic parser-parameters parser-state)))


;;
;;
;; singles
;;
;;


(defn process-single [add-element-fn token-key parser-state]
  (loop [pending-series-tokens (get-in parser-state [:pending-tokens token-key])
         state (assoc parser-state
                      :pending-tokens (dissoc (:pending-tokens parser-state) token-key))]
    (if (pos? (count pending-series-tokens))
      (let [current-token (nth pending-series-tokens 0)]
        (recur (subvec pending-series-tokens 1)
               (add-element-fn state current-token)))
      state)))


(defn add-hr [state current-token]
  (update state :elements conj (el/map->Hr {:start (:idx current-token)
                                              :end (+ (:idx current-token) (:length current-token))})))

(defrecord HrSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (if ((:el-type-allowed? parser-parameters) el/Hr)
      (process-single #(add-hr % %2) token/Hr parser-state)
      parser-state)))



(defn add-url [parser-parameters state current-token]
  (let [next-el-idx (or (apply min (filter #(> % (:idx current-token)) (keep :start (:elements state))))
                        (:children-end (:parent parser-parameters)))
        url-string (.trim (builder/get-match (:block-string parser-parameters) #".+?(?: |$)" (:idx current-token) next-el-idx))
        new-element (el/map->Url {:start (:idx current-token)
                      :end (+ (:idx current-token) (.-length url-string))
                      :url url-string})]
    (-> state
        (update :elements conj new-element)
        (assoc :pending-tokens (builder/filter-tokens #(not (< (:start new-element) (:idx %) (:end new-element)))
                                                      (:pending-tokens state))))))

(defrecord UrlSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (if ((:el-type-allowed? parser-parameters) el/Url)
      (process-single #(add-url parser-parameters % %2) token/Url parser-state)
      parser-state)))



;; TODO page children stuff
(defn add-attribute [parser-parameters state current-token]
  ;; attribute cannot be a child so assume root level
  (let [children  (builder/find-loose-children (:elements state) 0 (:idx current-token))
        children? (pos? (count children))
        page-name (builder/get-sub (:block-string parser-parameters) 0 (:idx current-token))]
    (-> state
        (cond-> (not (or (clojure.string/starts-with? page-name " ")
                         (clojure.string/index-of page-name "\n")))
          (builder/add-element (let [end-idx          (+ (:idx current-token) (:length current-token))
                                     first-child      (when children? (nth children 0))
                                     brackets? (and (= :page (:id first-child))
                                                           (identical? 0 (:start first-child))
                                                           (identical? (:idx current-token) (:end first-child)))
                                     common {:start 0
                                             :end   end-idx
                                             :brackets? brackets?}]
                                 (if brackets?
                                   (el/map->Attribute (assoc common :page-name           (:page first-child)
                                                             :children-start (:children-start first-child)
                                                             :children-end   (:children-end first-child)
                                                             :children       (:children first-child)))
                                   (el/map->Attribute (assoc common
                                           :children-start 0
                                           :children-end   (:idx current-token)
                                           :page-name           page-name
                                                                  ;; no need to process children further as it cannot contain anything of a lower priority anyway
                                           :children       children))))))
                             ;; only considering the first occurrence of ::
        (update :pending-tokens dissoc :attribute))))

(defrecord AttributeSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (if ((:el-type-allowed? parser-parameters) el/Attribute)
      (process-single #(add-attribute parser-parameters % %2) token/Attribute parser-state)
      parser-state)))

(def page-ns-split-re #"(?<!\\)/")
;; TODO escape namespace strings generally
(defn add-tag [state current-token]
  (update state :elements conj  (el/map->Tag
                                 {:start (:idx current-token)
                                  :end (+ (:idx current-token) (:length current-token))
                                  :page-name (:text current-token)
                                  :namespaces (let [strings (clojure.string/split (:text current-token) page-ns-split-re) ]
                                                (cond-> strings
                                                  (clojure.string/blank? (peek strings)) pop))})))
(defrecord TagSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters parser-state]
    (if ((:el-type-allowed? parser-parameters) el/Tag)
      (process-single #(add-tag % %2) token/Tag parser-state)
      parser-state)))
