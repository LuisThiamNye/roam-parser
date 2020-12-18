(ns roam-parser.core)

(defn j-time
  ([name x]
   (if false
     (do (.log js/console name)
         (j-time x))
     (x)))
  ([x]
   (if false
     (do (.time js/console "time")
         (let [result (x)]
           (.timeEnd js/console "time")
           result))
     (x))))

(def elements {:page {:start "[["
                      :end "]]"
                      :fn (fn process [string]
                            (let [idx (.indexOf (-> elements :page :start))]
                              4))
                      :allowed-children #{}}
               :bold {:start-end "**"
                      :allowed-children #{}}
               :italic {:start-end "__"
                        :allowed-children #{}}
               :highlight {:start-end "^^"
                           :allowed-children #{}}})

(def style-markers {:italic "__"
                    :bold "**"
                    :highlight "^^"})
(def style-marks {"__" :italic
                  "**" :bold
                  "^^" :highlight})

(defn parse-text [text]
  (let []))

(def delimiters [{:id :hiccup
                  :regex #"(?<!\n)^:hiccup(?<hiccupOpen>(?:.|\n)*)"
                  :length 7}
                 {:id :blockquote
                  :regex #"(?<!\n)^(?<blockquoteOpen>>|\[\[>\]\]|#\[\[>\]\])(?=.)"
                  :length nil}
                 {:id :hr
                  :regex #"^---$"
                  :length 3}
                 {:id :codeblock
                  :regex #"(?<!`)(?:(?<codeblockDual>`{3}(?!.*?`{3}))|(?<codeblockClose>`{3,}))(?!`)"
                  :length 3}
                 {:id :mono
                  :regex #"`+"
                  :length 1}
                 {:id :curly
                  :regex #"\{+|\}+"
                  :length 1}
                 {:id :attribute
                  :regex #"^(?<attribute>\S(?:[^`\{\}\n]*?\S)?)::"
                  :length nil}
                 {:id :latex
                  :regex #"(?<!\$)(?:(?<latexOpen>(?<!\S)\${2}(?=\S))|(?<latexClose>(?<=\S)\${2}(?!\S))|(?<latexDual>(?<=\S)\${2}(?=\S)))(?!\$)"
                  :length 2}
                 {:id :square
                  :regex #"\[+|\]+"
                  :length 1}
                 {:id :round
                  :regex #"\(+|\)+"
                  :length 1}
                 {:regex #"(?<!\*)(?:(?<boldOpen>(?<!\S)\*{2,}(?=\S))|(?<boldClose>(?<=\S)\*{2,}(?!\S))|(?<boldDual>(?<=\S)\*{2,}(?=\S)))(?!\*)"
                  :id :bold
                  :find-id (fn [token ^js groups fallback]
                             (if-let  [direction (cond (. groups -boldOpen) :open
                                                       (. groups -boldClose) :close
                                                       (. groups -boldDual) :dual)]
                               {:id :bold
                                :direction direction}
                               ((first fallback) token groups (rest fallback))))
                  :d {:boldOpen :open
                      :boldClose :close}
                  :length 2}
                 {:regex #"(?<!_)(?:(?<italicOpen>(?<!\S)_{2,}(?=\S))|(?<italicClose>(?<=\S)_{2,}(?!\S))|(?<italicDual>(?<=\S)_{2,}(?=\S)))(?!_)"
                  :id :italic
                  :length 2}
                 {:regex #"(?<!\^)(?:(?<highlightOpen>(?<!\S)\^{2,}(?=\S))|(?<highlightClose>(?<=\S)\^{2,}(?!\S))|(?<highlightDual>(?<=\S)\^{2,}(?=\S)))(?!\^)"
                  :id :highlight
                  :length 2}
                 {:id :tag
                  :regex #"(?<!#)#(?=\S)"
                  :length 1}
                 {:id :image
                  :regex #"!(?=\[)"
                  :length 1}])

(defn probe [x] (.log js/console x) x)

(def inline-re (js/RegExp. (let [trim-ends #(.slice % 1 -1)]
                             (reduce #(str %1 "|" %2)
                                     (map #(trim-ends (str (:regex %)))
                                          delimiters)))
                           ;; modifier flags
                           "gm"))

(defn parse-inline [string]
  (let [matches (.matchAll string inline-re)
        all-tokens (loop [match-stack (for [m matches]
                                        {:idx (.-index m)
                                         :group (loop [rest-groups (.entries js/Object (.-groups m))]
                                                  (if rest-groups
                                                    (let [[name value] (first rest-groups)]
                                                      (if value
                                                        [name value]
                                                        (recur (next rest-groups))))
                                                    nil))
                                         :text (first m)})
                          tbid (reduce #(assoc % (:id %2) [])
                                       {} delimiters)]
                     (if (seq match-stack)
                       (let [m (first match-stack)
                             length (count (:text m))
                             idx (:idx m)
                             group-name (first (:group m))
                             type (if group-name
                                    (let [[element direction] (rest (re-find #"(.*?)([A-Z])[a-z]+$" group-name))]
                                      {:id (keyword (or element group-name))
                                       :direction (case direction
                                                    "O" :open
                                                    "C" :close
                                                    :dual)
                                       :text (last (:group m))})
                                    (case (first (:text m))
                                      "(" {:id :round
                                           :direction :open}
                                      ")" {:id :round
                                           :direction :close}
                                      "[" {:id :square
                                           :direction :open}
                                      "]" {:id :square
                                           :direction :close}
                                      "{" {:id :curly
                                           :direction :open}
                                      "}" {:id :curly
                                           :direction :close}
                                      "`" {:id :mono
                                           :direction :dual}
                                      "-" {:id :hr
                                           :direction :dual}
                                      "#" {:id :tag
                                           :direction :open}
                                      "!" {:id :image
                                           :direction :open}
                                      :?))]
                         (recur (next match-stack)
                                (update tbid (:id type)
                                        conj (merge type
                                                    {:next-idx (+ idx length)
                                                     :length length
                                                     :idx idx}))))
                       tbid))
        add-opener #(conj % {:idx (:idx %2) :length (:length %2) :tag %3})
        process-layer (fn process-layer [tokens rules]
                        (let [process-token (fn [local-rules all-pending-tokens all-result]
                                              (let [id (first local-rules)
                                                    use-greedy (not (some #{id} #{:round :square :curly}))
                                                    use-heavy (boolean (some #{id} #{:curly}))
                                                    mode (when (some #{id} #{:hr :attribute :tag}) :single)
                                                    heavy-name :render
                                                    heavy-length 2
                                                    delimiter-length (:length (first (filter #(= (:id %) id) delimiters)))]
                                                (if (= :single mode)
                                                  (loop [ts (get all-pending-tokens id)
                                                         result all-result]
                                                    (if (seq ts)
                                                      (let [current-token (first ts)]
                                                        (recur (next ts)
                                                               (conj result (case id
                                                                              :hr {:start (:idx current-token)
                                                                                   :end (+ (dec (:length current-token)) (:idx current-token))
                                                                                   :id id}
                                                                              :attribute {:start (:idx current-token)
                                                                                          :end (+ (dec (:idx current-token)) (:length current-token))
                                                                                          :id id
                                                                                          :page (:text current-token)}))))
                                                      [result (dissoc all-pending-tokens id)]))
                                                  (loop [ts (get all-pending-tokens id)
                                                         result all-result
                                                         openers []
                                                         pending-tokens all-pending-tokens
                                                         is-active (not use-heavy)]
                                                    (if (and (seq pending-tokens) (seq ts))
                                                      (if-let [current-token (first ts)]
                                                        (let [{:keys [id direction next-idx length idx]} current-token
                                                              next-up (next ts)]

                                                          (if (= :open direction)
                                                            (recur next-up
                                                                   result
                                                                   (if is-active
                                                                     (add-opener openers current-token nil)
                                                                     (if (>= length heavy-length)
                                                                       (add-opener openers current-token :heavy)
                                                                       openers))
                                                                   pending-tokens
                                                                   (or (not use-heavy) (>= length heavy-length)))

                                                            ;; found potential closer
                                                            (if-let [partner (last openers)]
                                                              ;; found a matching pair, add to results
                                                              (let [is-heavy (= :heavy (:tag partner))]
                                                                (if (or (not is-heavy) (>= length heavy-length))
                                                                  (let [start-idx (if use-greedy
                                                                                    (+ (:idx partner) delimiter-length)
                                                                                    (+ (:idx partner) (:length partner)))
                                                                        is-greedy (and use-greedy)
                                                                        partner-length (if is-heavy heavy-length (:length partner))
                                                                        end-idx (if is-greedy
                                                                                  (+ idx (- length delimiter-length))
                                                                                  idx)
                                                                        loose-children (filter #(<= start-idx (:start %) idx) result)
                                                                        next-result (if (seq loose-children)
                                                                                      (filter #(not (<= start-idx (:start %) idx)) result)
                                                                                      result)
                                                                        filter-tokens (fn [f] (loop [x pending-tokens
                                                                                                     ks (keys pending-tokens)]
                                                                                                (if (seq ks)
                                                                                                  (recur (update x (first ks) (partial filter f))
                                                                                                         (next ks))
                                                                                                  x)))
                                                                        rest-openers (pop openers)
                                                                        delim-count (if use-greedy 1 (.floor js/Math (/ (min (:length partner) length) delimiter-length)))
                                                                        total-length (* delim-count delimiter-length)]
                                                                    (if-let  [squares (if (= :round id) (first (filter #(= (:last %) (- start-idx delimiter-length 1)) result))
                                                                                          nil)]
                                                                      ;; make alias
                                                                      (recur  (cond->> next-up
                                                                                (and (not use-greedy)
                                                                                     (>= length (if use-heavy
                                                                                                  (+ delimiter-length heavy-length)
                                                                                                  (* 2 delimiter-length))))
                                                                                (cons (-> current-token
                                                                                          (update :length - delimiter-length)
                                                                                          (update :idx + delimiter-length))))
                                                                              (conj (remove #(= (:start %) (:start squares)) next-result)
                                                                                    {:start (- (:start squares) (dec (:count squares)))
                                                                                     :end end-idx
                                                                                     :last (+ total-length (dec end-idx))
                                                                                     :id :alias
                                                                                     :is-image (boolean (seq (filter #(= (:idx %) (- (:start squares) (inc (:count squares)))) (get pending-tokens :image))))
                                                                                     :description-children (if (> (:count squares) 1)
                                                                                                             [(-> squares
                                                                                                                  (update :count dec)
                                                                                                                  (update :last dec))]
                                                                                                             (:children squares))
                                                                                     :link-children (let [new-children (concat (process-layer (filter-tokens #(< (:idx partner) (:idx %) idx))
                                                                                                                                              (rest local-rules))
                                                                                                                               loose-children)]
                                                                                                      (if (> delim-count 1)
                                                                                                        [{:start start-idx
                                                                                                          :end end-idx
                                                                                                          :last (+ total-length (- end-idx 2))
                                                                                                          :count (dec delim-count)
                                                                                                          :id id
                                                                                                          :children new-children}]
                                                                                                        new-children))})
                                                                              (cond-> rest-openers
                                                                                ;; split opener into higher level, outer opener if long enough
                                                                                (and (not use-greedy) (not is-heavy)
                                                                                     (>= partner-length (* 2 delimiter-length)))
                                                                                (conj (update partner :length - delimiter-length)))
                                                                              ;; update pending tokens
                                                                              (filter-tokens #(not (<= (:idx partner) (:idx %) idx)))
                                                                              ;; keep active if opener stack not empty
                                                                              (or (not is-heavy)
                                                                                  (boolean (seq rest-openers))))
                                                                      (recur  (cond->> next-up
                                                                                (and (not use-greedy)
                                                                                     (>= length (if use-heavy
                                                                                                  (+ delimiter-length heavy-length)
                                                                                                  (+ total-length delimiter-length))))
                                                                                (cons (-> current-token
                                                                                          (update :length - total-length)
                                                                                          (update :idx + total-length))))
                                                                              (conj next-result {:start start-idx
                                                                                                 :end end-idx
                                                                                                 :last (+ total-length (dec end-idx))
                                                                                                 :count delim-count
                                                                                                 :id (if is-heavy heavy-name id)
                                                                                                 :children (concat (process-layer (filter-tokens #(< (:idx partner) (:idx %) idx))
                                                                                                                                  (rest local-rules))
                                                                                                                   loose-children)})
                                                                              (cond-> rest-openers
                                                                                ;; split opener into higher level, outer opener if long enough
                                                                                (and (not use-greedy) (not is-heavy)
                                                                                     (>= partner-length (+ total-length delimiter-length)))
                                                                                (conj (update partner :length - total-length)))
                                                                              ;; update pending tokens
                                                                              (filter-tokens #(not (<= (:idx partner) (:idx %) idx)))
                                                                              ;; keep active if opener stack not empty
                                                                              (or (not is-heavy)
                                                                                  (boolean (seq rest-openers))))))
                                                                  (recur  next-up
                                                                          result
                                                                          openers
                                                                          pending-tokens
                                                                          is-active)))
                                                              ;; no partner found
                                                              (if (= :close direction)
                                                                ;; discard
                                                                (recur  next-up
                                                                        result
                                                                        openers
                                                                        pending-tokens
                                                                        is-active)
                                                                ;; dual -> add as opener
                                                                (recur next-up
                                                                       result
                                                                       (add-opener openers current-token nil)
                                                                       pending-tokens
                                                                       is-active)))))
                                                        (recur  (next ts)
                                                                result
                                                                openers
                                                                pending-tokens
                                                                is-active))
                                                      [(cond-> result
                                                         ;; find [[]] and (())
                                                         (= :round id)
                                                         (identity)
                                                         (= :mono id)
                                                         (concat (keep identity (for [o openers]
                                                                                  (when (> (:length o) (* 2 delimiter-length))
                                                                                    {:start (+ (:idx o) delimiter-length)
                                                                                     :end (+ (:idx o) (- (:length o) delimiter-length))
                                                                                     :id id
                                                                                     :children "todo"}))))
                                                         (= :codeblock id)
                                                         (concat (for [o openers]
                                                                   {:start (inc (:idx o))
                                                                    :end (+ (:idx o) (dec (:length o)))
                                                                    :id :mono
                                                                    :children "todo: `"})))
                                                       pending-tokens])))))]
                          (loop [pending-rules rules
                                 all-result []
                                 all-pending-tokens tokens]
                            (if (and (seq pending-rules) (seq all-pending-tokens))
                              (let [[result next-pending-tokens] (process-token pending-rules all-pending-tokens all-result)]
                                (recur (rest pending-rules) result next-pending-tokens))
                              all-result))))
        hit-tokens (dissoc (select-keys all-tokens (for [[k v] all-tokens :when (seq v)] k))
                           :blockquote :hiccup)
        rules [:square :round :curly :mono :codeblock :attribute :bold]]
    (if-let [hc (first (:hiccup all-tokens))]
      {:id :hiccup
       :start 7
       :end (dec (count string))
       :children []}
      (if-let [bq (first (:blockquote all-tokens))]
        {:id :blockquote
         :start (:length bq)
         :end (dec (count string))
         :children (process-layer all-tokens rules)}
        (process-layer all-tokens rules)))))
