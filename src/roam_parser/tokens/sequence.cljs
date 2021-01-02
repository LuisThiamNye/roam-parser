(ns roam-parser.tokens.sequence (:require [roam-parser.tokens.token :as token]
                                          [clojure.string]
                                          [roam-parser.builder :as builder]))



(defprotocol TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]))

(defn process-children [token-seq parser-parameters]
  (let []
    (loop [token-seq-types (:token-seq-type-queue parser-parameters)
           element-acc []
           pending-tokens (:tokens parser-parameters)]
      (if (and (pos? (count token-seq-types)))
        (let [token-seq-type (nth token-seq-types 0)
              token-seq (token-seq-type. (get pending-tokens token-seq-type))
              other-pending-tokens (dissoc pending-tokens token-seq-type)]
          ;; TODO only allow some delimiters
          (if true
            (let [state (process-tokens token-seq parser-parameters other-pending-tokens element-acc)]
              (recur (subvec token-seq-types 1)
                     (:elements state)
                     (:pending-tokens state)))
            (recur (subvec token-seq-types 1) element-acc pending-tokens)))
        (builder/finalise-children (:parent parser-parameters) (:block-string parser-parameters) element-acc (:text-mode parser-parameters))))))



(defrecord CodeblockSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]
    ;; turn orphaned runs of ```` into coded backticks
    (reduce (fn [state o]
              (update state :elements conj
                      {:start          (:idx o)
                       :end            (+ (:idx o) (:length o))
                       :id             :code
                       :children-start (inc (:idx o))
                       :children-end   (dec (+ (:idx o) (:length o)))
                       :content        (apply str (repeat (- (:length o) 2) "`"))}))
            initial-state (:free-openers initial-state))))



(defn on-abandoned-close [current-results current-token]
  (case (:id current-token)
    :codeblock (conj current-results {:start (:idx current-token)
                                      :end (+ (:idx current-token) (:length current-token))
                                      :id :code
                                      :children-start (inc (:idx current-token))
                                      :children-end (dec (+ (:idx current-token) (:length current-token)))
                                      :content (apply str (repeat (- (:length current-token) 2) "`"))})
    current-results))

(defn process-paired [injections parser-parameters pending-tokens existing-elements]
  (let [close-delimiter  (:close-delimiter injections)]
    (loop [state (hash-map :these-pending-tokens
                           :pending-tokens
                           :elements existing-elements
                           :free-openers [])]
      (if (pos? (count (:these-pending-tokens state)))
        (let [current-token                  (nth (:these-pending-tokens state) 0)
              {:keys [direction]} current-token
              current-state                  (update state :these-pending-tokens subvec 1)]

          (recur (if (= :open direction)
             (update current-state :free-openers
                     builder/new-opener current-token (:tag current-token))
            ;; found potential closer
             (if-let [partner (peek (:free-openers current-state))]
              ;; found a matching pair, add to results
               (close-delimiter parser-parameters current-state current-token partner)
                ;; found a (potential) closer but no partner found
               (if (= :close direction)
                       ;; close -> see if anything should be done with orphaned closer
                 (update current-state :elements
                         on-abandoned-close current-token)
                       ;; dual -> add as opener
                 (update current-state :free-openers
                         builder/add-opener current-token))))))
        ;; no more tokens of this type to process -> return
        state))))

(defrecord BacktickSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]
    (process-paired {:close-delimiter (builder/close-ambiguous-fn 1)} parser-parameters pending-tokens existing-elements)))


;; TODO ignore any of length 1 if curly not allowed
(defrecord CurlySequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]
    (process-paired {:close-delimiter builder/close-curly} parser-parameters pending-tokens existing-elements)))



(defrecord SquareSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]
    (process-paired {:close-delimiter builder/close-square} parser-parameters pending-tokens existing-elements)))

(defrecord RoundSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]
    (process-paired {:close-delimiter builder/close-round} parser-parameters pending-tokens existing-elements)))

(defrecord LatexSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]))

(defrecord HighlightSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]))

(defrecord BoldSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]))

(defrecord ItalicSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]))


(defn process-single [add-element-fn parser-parameters pending-tokens existing-elements]
  (let [add-single-element (case :id-to-parse
                             :attribute (partial add-attribute parser-parameters)
                             :tag add-tag
                             :url (partial add-url parser-parameters)
                             (partial default-add-single-element :id-to-parse))]
    (loop [these-pending-tokens (get pending-tokens :id-to-parse)
           state {:pending-tokens (dissoc pending-tokens :id-to-parse)
                  :elements existing-elements}]
      (if (pos? (count these-pending-tokens))
        (let [current-token (nth these-pending-tokens 0)]
          (recur (subvec these-pending-tokens 1)
                 (add-element-fn state current-token)))
        state))))

(defrecord HrSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]))

(defn add-url [parser-parameters state current-token]
  (let [next-el-idx (or (apply min (filter #(> % (:idx current-token)) (keep :start (:elements state))))
                        (:children-end parent))
        url-string (.trim (get-match (:block-string parser-parameters) #".+?(?: |$)" (:idx current-token) next-el-idx))
        new-element {:start (:idx current-token)
                     :end (+ (:idx current-token) (.-length url-string))
                     :id :url
                     :url url-string}]
    (-> state
        (update :elements conj new-element)
        (assoc :pending-tokens (filter-tokens #(not (< (:start new-element) (:idx %) (:end new-element)))
                                              (:pending-tokens state))))))

(defrecord UrlSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]
    (process-single #(add-url parser-parameters % %2) pending-tokens existing-elements)))

(defn add-attribute [parser-parameters state current-token]
  ;; attribute cannot be a child so assume root level
  (let [children  (find-loose-children (:elements state) 0 (:idx current-token))
        children? (pos? (count children))
        page-name (get-sub (:block-string parser-parameters) 0 (:idx current-token))]
    (-> state
        (cond-> (not (or (clojure.string/starts-with? page-name " ")
                         (clojure.string/index-of page-name "\n")))
          (add-element (let [end-idx          (+ (:idx current-token) (:length current-token))
                             first-child      (when children? (nth children 0))
                             use-bracket-page (and (= :page (:id first-child))
                                                   (identical? 0 (:start first-child))
                                                   (identical? (:idx current-token) (:end first-child)))
                             common {:start 0
                                     :end   end-idx}]
                         (if use-bracket-page
                           (assoc common :page           (:page first-child)
                                  :id             :bracket-attribute
                                  :children-start (:children-start first-child)
                                  :children-end   (:children-end first-child)
                                  :children       (:children first-child))
                           (assoc common :id             :attribute
                                  :children-start 0
                                  :children-end   (:idx current-token)
                                  :page           page-name
                                                                  ;; no need to process children further as it cannot contain anything of a lower priority anyway
                                  :children       children)))))
                             ;; only considering the first occurrence of ::
        (update :pending-tokens dissoc :attribute))))

(defrecord AttributeSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]
    (process-single #(add-attribute parser-parameters % %2) pending-tokens existing-elements)))


(defn add-tag [state current-token]
  (update state :elements conj  {:start (:idx current-token)
                                 :end (+ (:idx current-token) (:length current-token))
                                 :id :tag
                                 :page (:text current-token)}))
(defrecord TagSequence [tokens]
  TokenSequenceProtocol
  (process-tokens [this parser-parameters pending-tokens existing-elements]
    (process-single #(add-tag % %2) pending-tokens existing-elements)))
