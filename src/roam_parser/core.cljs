(ns roam-parser.core (:require [clojure.set]
                               [clojure.string]
                               [goog.string]
                               [roam-parser.utils :as utils]
                               [roam-parser.render :as render]))

(defn probe [x] (.log js/console x) x)

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

(def rules [:codeblock :code :curly :hr :square :round :latex :highlight :bold :italic :url :attribute :tag])

(def escapable-char-regex #"[\\{}\[\]\(\)`\*_\^:#!\n>]|\${2}")

(def codeblock-re (js/RegExp #"^(?:(javascript|css|html|clojure|common lisp)\n|\n?)(.*(?:[^\n]|$))" "s"))
(def default-codeblock-lang "javascript")

(def style-el-definition (array-map :allowed-children #{:parenthetical :code :page :alias
                                                        :bold :italic :highlight :url}
                                    :killed-by #{:image :render :latex}
                                    :scope :inline))

(def element-definitions {:block (array-map :allowed-children #{:blockquote :hr :codeblock :code :render :page :bracket-tag
                                                                :alias :image :parenthetical :latex :bold :italic :highlight
                                                                :url :attribute :bracket-attribute :tag})
                          :blockquote (array-map :allowed-children #{:page :render :image :bold :highlight :italic
                                                                     :alias :parenthetical :hr :code :url :latex
                                                                     :bracket-tag :tag :block-ref})
                          :codeblock (array-map)
                          :code (array-map :killed-by #{:codeblock})
                          :render (array-map :allowed-children #{:page :block-ref :curly}
                                             :killed-by #{:codeblock
                                                          :code}
                                             :delimiters #{:curly}
                                             :invisible? :true)
                          :curly (array-map :allowed-children #{:page :block-ref :curly}
                                            :killed-by #{:codeblock
                                                         :code})
                          :page (array-map :allowed-children #{:page :bold :highlight :italic}
                                           :delimiters #{:square}
                                           :scope :inline
                                           :killed-by #{:image :alias})
                          :bracket-tag (array-map :allowed-children #{:page :bold :highlight :italic}
                                                  :delimiters #{:square}
                                                  :scope :inline
                                                  :killed-by #{:image :alias})
                          :alias (array-map :allowed-children #{:latex :code :image
                                                                :bold :highlight :italic}
                                            :delimiters #{:square :round})
                          :image (array-map :allowed-children #{:bold :highlight :italic
                                                                :image}
                                            :delimiters #{:square :round})
                          ;; dummy element; never created
                          :alias-destination (array-map :allowed-children #{:page :block-ref}
                                                        :killed-by #{:alias}
                                                        :scope :inline
                                                        :invisible? :true)
                          :parenthetical (array-map :allowed-children #{:code :render :url :latex
                                                                        :bold :highlight :italic
                                                                        :page :alias :parenthetical :image
                                                                        :bracket-tag :tag :block-ref}
                                                    :killed-by #{:codeblock}
                                                    :delimiters #{:round})
                          :latex (array-map :killed-by #{:codeblock})
                          :bold style-el-definition :italic style-el-definition :highlight style-el-definition

                          ;; singles

                          :hr (array-map)
                          :url (array-map)
                          :attribute (array-map :allowed-children #{:bold :italic :highlight}
                                                :killed-by #{:url :latex :block-ref :parenthetical :image :alias :bracket-tag :render :code})
                          :bracket-attribute (array-map :allowed-children #{:bold :italic :highlight :page}
                                                        :killed-by #{:image :alias})
                          ;; tags cannot overlap with other delimiters due to regex
                          :tag (array-map)})

(def delimiters [(array-map :id :hiccup
                            :regex #"(?<!\n)^:hiccup(?<hiccup>(?:.|\n)*)"
                            :length 7)
                 (array-map :id :blockquote
                            :regex #"(?<!\n)^(?<blockquot>>|\[\[>\]\]|#\[\[>\]\])(?=.)")
                 (array-map :id :hr
                            :flags #{:single}
                            :regex #"^---$"
                            :length 3)
                 (array-map :id :codeblock
                            :flags #{:greedy}
                            :regex #"(?<!`)(?:(?<codeblockDual>`{3}(?!.*?`{3}))|(?<codeblockClose>`{3,}))(?!`)"
                            :length 3)
                 (array-map :id :code
                            :flags #{:greedy}
                            :regex #"`+"
                            :length 1)
                 (array-map :id :curly
                            :flags #{:bracket}
                            :regex #"\{+|\}+"
                            :length 1)
                 (array-map  :id :attribute
                             :flags #{:single}
                             :regex #"(?<=\S)::")
                 (array-map :id :latex
                            :regex #"(?<!\$)(?:(?<latexOpen>(?<!\S)\${2}(?=\S))|(?<latexClose>(?<=\S)\${2}(?!\S))|(?<latexDual>(?<=\S)\${2}(?=\S)))(?!\$)"
                            :length 2)
                 (array-map :id :square
                            :flags #{:bracket}
                            :regex #"\[+|\]+"
                            :length 1)
                 (array-map  :id :round
                             :flags #{:bracket}
                             :regex #"\(+|\)+"
                             :length 1)
                 (array-map :id :bold
                            :flags #{:greedy}
                            :regex #"(?<!\*)(?:(?<boldOpen>(?<!\S)\*{2,}(?=\S))|(?<boldClose>(?<=\S)\*{2,}(?!\S))|(?<boldDual>(?<=\S)\*{2,}(?=\S)))(?!\*)"
                            :length 2)
                 (array-map :id :italic
                            :flags #{:greedy}
                            :regex #"(?<!_)(?:(?<italicOpen>(?<!\S)_{2,}(?=\S))|(?<italicClose>(?<=\S)_{2,}(?!\S))|(?<italicDual>(?<=\S)_{2,}(?=\S)))(?!_)"
                            :length 2)
                 (array-map :id :highlight
                            :flags #{:greedy}
                            :regex #"(?<!\^)(?:(?<highlightOpen>(?<!\S)\^{2,}(?=\S))|(?<highlightClose>(?<=\S)\^{2,}(?!\S))|(?<highlightDual>(?<=\S)\^{2,}(?=\S)))(?!\^)"
                            :length 2)
                 (array-map :id :tag
                            :flags #{:single}
                            :regex #"(?<=[ \(\[:]|^)#(?<tag>[\w-_/@:]+(?<!::)(?!:(?!:)))")
                 (array-map :id :hash
                            :regex #"#(?=\[)"
                            :length 1)
                 (array-map :id :url
                            :flags #{:single}
                            :regex #"(?<![\w\.-])(?<url>https?://[a-zA-Z\d-\.]+\.[a-zA-Z\d-]{2,}|www\.[a-zA-Z\d-\.]+\.[a-zA-Z\d-]{2,})")
                 (array-map  :id :!
                             :regex #"!(?=\[)"
                             :length 1)
                 (array-map  :id :newline
                             :regex #"\n+")])

;; TODO make eager
(def allowed-delimiters (apply merge (for [[el-id el-data] element-definitions]
                                       {el-id (apply clojure.set/union (for [id (:allowed-children el-data)]
                                                                         (or (:delimiters (get element-definitions id))
                                                                             #{id})))})))

(defn symbol-data-from-group [group]
  (let [[element direction] (rest (re-find #"(.*?)([A-Z])[a-z]+$" group))]
    {:id (keyword (or element group))
     :direction (case direction
                  "O" :open
                  "C" :close
                  :dual)}))

(defn symbol-data-from-char [^string first-char]
  (case first-char
    ")" {:id :round
         :direction :close}
    "(" {:id :round
         :direction :open}
    "]" {:id :square
         :direction :close}
    "[" {:id :square
         :direction :open}
    "{" {:id :curly
         :direction :open}
    "}" {:id :curly
         :direction :close}
    ":" {:id :attribute}
    "`" {:id :code}
    "!" {:id :!}
    "#" {:id :hash}
    "\n" {:id :newline}
    "-" {:id :hr}
    nil))

(defn re-to-str [re] (.slice (str re) 1 -1))
(defn to-regex [^string str] (js/RegExp. str "gm"))
(def inline-re (to-regex (reduce #(str %1 "|" %2) (str "\\\\(?:" (re-to-str escapable-char-regex) ")")
                                 (map #(re-to-str (:regex %))
                                      delimiters))))

(def str-replace-re (to-regex (str "\\\\(?<escape>" (re-to-str escapable-char-regex) ")")))
(defn escape-str [^string string] (.replace string str-replace-re "$<escape>"))

(defn filter-tokens [f tks]
  (loop [x  tks
         ks (keys tks)]
    (if-let [tk-key (first ks)]
      (recur (update x tk-key (partial utils/eager-filter f))
             (next ks))
      x)))

(defn find-token [tks id idx] (some #(when (identical? (:idx %) idx) %) (get tks id)))
(defn add-opener
  ([openers new] (conj openers {:idx (:idx new) :length (:length new)}))
  ([openers new tag] (conj openers {:idx (:idx new) :length (:length new) :tag tag})))


(defn parse-inline [^string string]
  (let [matches (.matchAll string inline-re)
        ;; map of delimiters and their occurrences
        all-tokens (dissoc (loop [output (transient (hash-map))]
                             (let [iter-item (.next matches)
                                   m (.-value iter-item)]
                               (if ^boolean (.-done iter-item)
                                 (persistent! output)
                                 (let [text (nth m 0)
                                       groups (.entries js/Object (.-groups m))
                                       group-count (.-length groups)
                                       group (loop [i 0]
                                               (if (< i group-count)
                                                 (let [[name value] (nth groups i)]
                                                   (if (nil? value)
                                                     (recur (inc i))
                                                     [name value]))
                                                    ;; does not match any group
                                                 [nil nil]))
                                       length (.-length text)
                                       idx (.-index m)
                                       group-name (nth group 0)
                                       symbol-data (if (nil? group-name)
                                                     (or (symbol-data-from-char (nth text 0))
                                                         {:id :ignored})
                                                     (assoc (symbol-data-from-group group-name)
                                                            :text (peek group)))
                                       new-token (merge symbol-data
                                                        {:length length
                                                         :idx idx})
                                       old-data (get output (:id symbol-data))]
                                   (recur (assoc! output (:id symbol-data)
                                                  (if (nil? old-data)
                                                    (vector new-token)
                                                    (conj old-data new-token))))))))
                           :ignored)
        get-sub (fn get-sub ([start end no-escape] (subs string start end))
                  ([start end]
                   (-> (subs string start end)
                       escape-str)))
        get-match (fn get-match [re start end]
                    (re-find re (subs string start end)))
        add-text-nodes (fn add-text-nodes [elements start end]
                         (let [add-text-node (fn add-text-node [coll start end]
                                               (cond-> coll
                                                 (< start end) (conj {:id :text
                                                                      :content (get-sub start end)})))]
                           (loop [bounding-els (sort-by :start elements)
                                  section-start start
                                  output []]
                             (if (seq bounding-els)
                               (let [this-el (first bounding-els)]
                                 (recur (next bounding-els)
                                        (:end this-el)
                                        (-> output
                                            (add-text-node section-start (:start this-el))
                                            (conj this-el))))
                               (add-text-node output section-start end)))))
        process-slice (fn process-slice [tokens rules existing-elements slice-end parent-id]
                        (let [allowed-ids (filter (get allowed-delimiters parent-id) rules)
                              process-token (fn process-token [delimiter-ids all-pending-tokens all-result]
                                              (let [id (first delimiter-ids)
                                                    delimiter-data (some #(when (= (:id %) id) %) delimiters)
                                                    use-greedy (contains? (:flags delimiter-data) :greedy)
                                                    single? (contains? (:flags delimiter-data) :single)
                                                    bracket? (contains? (:flags delimiter-data) :bracket)
                                                    delimiter-length (:length delimiter-data)]
                                                (if single?

                                                  ;; process single delimiter types
                                                  (loop [ts (get all-pending-tokens id)
                                                         result all-result
                                                         pending-tokens (dissoc all-pending-tokens id)]
                                                    (if-let [current-token (first ts)]
                                                      (let [[new-result
                                                             next-tokens] (case id
                                                                            ;; attribute cannot be a child so assume root level
                                                                            :attribute [(let [children (filter #(< (:start %) (:idx current-token)) result)
                                                                                              sibling-tokens (if (seq children)
                                                                                                               (filter #(> (:start %) (:idx current-token)) result)
                                                                                                               result)
                                                                                              page-name (get-sub 0 (:idx current-token))]
                                                                                          (if (or (clojure.string/starts-with? page-name " ") (clojure.string/index-of page-name "\n"))
                                                                                            result
                                                                                            (conj sibling-tokens (let [end-idx (+ (:idx current-token) (:length current-token))
                                                                                                                       first-child (first children)
                                                                                                                       use-bracket-page (and (= :page (:id first-child))
                                                                                                                                             (identical? 0 (:start first-child))
                                                                                                                                             (identical? (:idx current-token) (:end first-child)))]
                                                                                                                   (merge {:start 0
                                                                                                                           :end end-idx}
                                                                                                                          (if use-bracket-page
                                                                                                                            {:page (:page first-child)
                                                                                                                             :id :bracket-attribute
                                                                                                                             :children-start (:children-start first-child)
                                                                                                                             :children-end (:children-end first-child)
                                                                                                                             :children (:children first-child)}
                                                                                                                            {:id :attribute
                                                                                                                             :children-start 0
                                                                                                                             :children-end (:idx current-token)
                                                                                                                             :page page-name
                                                                                                              ;; no need to process children further as it cannot contain anything of a lower priority anyway
                                                                                                                             :children children}))))))
                                                                         ;; only considering the first occurrence of ::
                                                                                        (dissoc pending-tokens :attribute)]

                                                                            :tag [(conj result {:start (:idx current-token)
                                                                                                :end (+ (:idx current-token) (:length current-token))
                                                                                                :id :tag
                                                                                                :page (:text current-token)})
                                                                                  pending-tokens]

                                                                            :url (let [next-el-idx (or (apply min (keep :start result))
                                                                                                       slice-end)
                                                                                       url-string (.trim (get-match #".+?(?: |$)" (:idx current-token) next-el-idx))
                                                                                       new-element {:start (:idx current-token)
                                                                                                    :end (+ (:idx current-token) (.-length url-string))
                                                                                                    :id :url
                                                                                                    :url url-string}]
                                                                                   [(conj result new-element)
                                                                                    (filter-tokens #(not (< (:start new-element) (:idx %) (:end new-element))) pending-tokens)])

                                                                            [(conj result {:start (:idx current-token)
                                                                                           :end (+ (:idx current-token) (:length current-token))
                                                                                           :id id})
                                                                             pending-tokens])]
                                                        (recur (next ts)
                                                               new-result
                                                               next-tokens))
                                                      [result pending-tokens]))

                                                  ;; process paired delimiter types
                                                  (loop [tks (get all-pending-tokens id)
                                                         result all-result
                                                         openers []
                                                         pending-tokens all-pending-tokens]
                                                    (if-let [current-token (first tks)]
                                                      (let [{:keys [id direction length idx]} current-token
                                                            next-up (next tks)]

                                                        (if (= :open direction)
                                                          (recur next-up
                                                                 result
                                                                 (if (and (= :curly id) (empty? openers) (identical? 1 length))
                                                                   openers
                                                                   (add-opener openers current-token (:tag current-token)))
                                                                 pending-tokens)

                                                          ;; found potential closer
                                                          (if-let [partner (peek openers)]
                                                            ;; found a matching pair, add to results
                                                            (let [quantity (if use-greedy 1
                                                                               (.floor js/Math (/ (min (:length partner) length) delimiter-length)))
                                                                  thickness (* quantity delimiter-length)
                                                                  start-idx (if use-greedy
                                                                              (:idx partner)
                                                                              (- (+ (:idx partner) (:length partner)) thickness))
                                                                  greedy? use-greedy
                                                                  partner-length (:length partner)
                                                                  end-idx (if greedy?
                                                                            (+ idx length)
                                                                            (+ idx thickness))
                                                                  loose-children (filter #(< start-idx (:start %) idx) result)
                                                                  killed? (fn killed? [element-id]
                                                                              (let [el-definition (get element-definitions element-id)]
                                                                                (or (when-let [killed-by (:killed-by el-definition)]
                                                                                      (some killed-by (keep :id loose-children)))
                                                                                    (when (= :inline (:scope el-definition))
                                                                                      (seq (filter #(< start-idx (:idx %) end-idx) (:newline pending-tokens)))))))
                                                                  sibling-elements (if (seq loose-children)
                                                                                     (filter #(not (< start-idx (:start %) idx)) result)
                                                                                     result)
                                                                  rest-openers (pop openers)
                                                                  sibling-tokens (filter-tokens #(not (<= (:idx partner) (:idx %) idx)) pending-tokens)
                                                                  container-element (fn [id new-thickness props]
                                                                                      (merge props
                                                                                             {:children-start (+ start-idx new-thickness)
                                                                                              :children-end (- end-idx new-thickness)
                                                                                              :id id}))]
                                                              (if bracket?
                                                                (let [next-tokens (cond->> next-up
                                                                                    (and (>= length (+ thickness delimiter-length)))
                                                                                    (cons (-> current-token
                                                                                              (update :length - thickness)
                                                                                              (update :idx + thickness))))
                                                                      next-openers (cond-> rest-openers
                                                                                     ;; split opener into higher level, outer opener if long enough
                                                                                     (and (>= partner-length (+ thickness delimiter-length)))
                                                                                     (conj (update partner :length - thickness)))
                                                                      inner-start (+ start-idx thickness)
                                                                      inner-end (- end-idx thickness)
                                                                      not-empty (pos? (- inner-end inner-start))
                                                                      [next-results
                                                                       next-sibling-tokens] (let [test-if-alias (fn []
                                                                                                                  (contains? #{:alias :image} (:tag partner)))
                                                                                                  try-alias (fn []
                                                                                                              (let [squares (first (filter #(identical? (:end %) start-idx) result))
                                                                                                                    element-id (:tag partner)
                                                                                                                    [dest-type
                                                                                                                     dest] (let [children-start inner-start
                                                                                                                                 children-end inner-end]
                                                                                                                             (cond
                                                                                                                               (and (identical? quantity 3)
                                                                                                                                    not-empty) [:block-ref
                                                                                                                                                (get-sub children-start children-end :no-escape)]
                                                                                                                               (and (identical? quantity 1)
                                                                                                                                    (or not-empty
                                                                                                                                        (= :alias (:tag partner))))
                                                                                                                               (let [children (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                            pending-tokens)
                                                                                                                                                             (rest delimiter-ids)
                                                                                                                                                             loose-children
                                                                                                                                                             children-end
                                                                                                                                                             :alias-destination)
                                                                                                                                     first-child (first children)]
                                                                                                                                 (if (and (identical? 1 (count children))
                                                                                                                                          (= :page (:id first-child))
                                                                                                                                          (= children-start (:start first-child))
                                                                                                                                          (= children-end (:end first-child)))
                                                                                                                                   [:page
                                                                                                                                    (:page first-child)]
                                                                                                                                   [:url
                                                                                                                                    (get-sub children-start children-end)]))))]

                                                                                                                (if (and dest-type (not (killed? element-id)))
                                                                                                                  [(conj (remove #(identical? (:start %) (:start squares)) sibling-elements)
                                                                                                                         {:start (cond-> (:start squares)
                                                                                                                                   (= :image element-id) dec)
                                                                                                                          :divider-idx (dec (:end squares))
                                                                                                                          :end end-idx
                                                                                                                          :id element-id
                                                                                                                          :children-start (:children-start squares)
                                                                                                                          :children-end (:children-end squares)
                                                                                                                          :children (:children squares)
                                                                                                                          :destination-type dest-type
                                                                                                                          :destination dest})
                                                                                                                   sibling-tokens]
                                                                                                                  [result
                                                                                                                   pending-tokens])))]
                                                                                              (cond
                                                                                                (= id :square) (let [following-round (let [r (find-token pending-tokens :round end-idx)]
                                                                                                                                       (when (= :open (:direction r)) r))
                                                                                                                     could-be-page (> quantity 1)]
                                                                                                                 (if (or following-round could-be-page)
                                                                                                                   (if following-round
                                                                                                                     ;; this is potential alias text
                                                                                                                     (let [alias-id (if (find-token pending-tokens :! (dec start-idx))
                                                                                                                                      :image
                                                                                                                                      :alias)]
                                                                                                                       (if (or (killed? alias-id) (and (not not-empty) (= :alias alias-id)))
                                                                                                                         [result
                                                                                                                          pending-tokens]
                                                                                                                         [(conj sibling-elements {:end end-idx
                                                                                                                                                  :count 1
                                                                                                                                                  :start start-idx
                                                                                                                                                  :id id
                                                                                                                                                  :children-start (+ start-idx delimiter-length)
                                                                                                                                                  :children-end (- end-idx delimiter-length)
                                                                                                                                                  :children (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                                          pending-tokens)
                                                                                                                                                                           (rest delimiter-ids)
                                                                                                                                                                           loose-children
                                                                                                                                                                           (- end-idx delimiter-length)
                                                                                                                                                                           alias-id)})
                                                                                                                          ;; mark opening round as potential alias
                                                                                                                          (assoc sibling-tokens
                                                                                                                                 :round (sort-by :idx (conj
                                                                                                                                                       (remove #(identical? (:idx %) (:idx following-round)) (:round sibling-tokens))
                                                                                                                                                       (assoc following-round :tag alias-id))))]))
                                                                                                                     ;; this is a page [[ ]]
                                                                                                                     (let [page-id (if (find-token pending-tokens :hash (dec start-idx))
                                                                                                                                     :bracket-tag
                                                                                                                                     :page)]
                                                                                                                       (if (and (killed? page-id) (not not-empty))
                                                                                                                         [result
                                                                                                                          pending-tokens]
                                                                                                                         [(conj sibling-elements {:end end-idx
                                                                                                                                                  :count 1
                                                                                                                                                  :start (cond-> start-idx
                                                                                                                                                           (= :bracket-tag page-id) dec)
                                                                                                                                                  :id page-id
                                                                                                                                                  :children-start inner-start
                                                                                                                                                  :children-end inner-end
                                                                                                                                                  :page (get-sub (+ start-idx thickness) (- end-idx thickness))
                                                                                                                                                  :children (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                                          pending-tokens)
                                                                                                                                                                           (rest delimiter-ids)
                                                                                                                                                                           loose-children
                                                                                                                                                                           inner-end
                                                                                                                                                                           page-id)})
                                                                                                                          sibling-tokens])))
                                                                                                                   ;; this is a single pair of brackets to discard
                                                                                                                   [result
                                                                                                                    pending-tokens]))
                                                                                                (test-if-alias) (try-alias)
                                                                                                (and not-empty
                                                                                                     (= id :round)) (if (and (> thickness delimiter-length) (not (killed? :parenthetical)))
                                                                                                                      [(conj sibling-elements {:start start-idx
                                                                                                                                               :end end-idx
                                                                                                                                               :count 1
                                                                                                                                               :id :parenthetical
                                                                                                                                               :children-start inner-start
                                                                                                                                               :children-end inner-end
                                                                                                                                               :children (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                                       pending-tokens)
                                                                                                                                                                        (rest delimiter-ids)
                                                                                                                                                                        loose-children
                                                                                                                                                                        inner-end
                                                                                                                                                                        :parenthetical)})
                                                                                                                       sibling-tokens]
                                                                                                     ;;  a normal pair of ( ) brackets, discard and surface children as an isolated bunch
                                                                                                                      [(process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                     pending-tokens)
                                                                                                                                      (rest delimiter-ids)
                                                                                                                                      result
                                                                                                                                      inner-end
                                                                                                                                      parent-id)
                                                                                                                       sibling-tokens])
                                                                                                (and not-empty
                                                                                                     (= id :curly)) [(conj sibling-elements (if (and (identical? 1 (count openers))
                                                                                                                                                     (>= thickness 2)
                                                                                                                                                     (not (killed? :render)))
                                                                                                                                              {:start (- (+ (:idx partner) (:length partner)) 2)
                                                                                                                                               :end (+ idx 2)
                                                                                                                                               :count 1
                                                                                                                                               :id :render
                                                                                                                                               :children-start inner-start
                                                                                                                                               :children-end inner-end
                                                                                                                                               :children (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                                       pending-tokens)
                                                                                                                                                                        (rest delimiter-ids)
                                                                                                                                                                        loose-children
                                                                                                                                                                        inner-end
                                                                                                                                                                        :render)}
                                                                                                                                              {:start start-idx
                                                                                                                                               :end end-idx
                                                                                                                                               :count quantity
                                                                                                                                               :id id
                                                                                                                                               :children-start inner-start
                                                                                                                                               :children-end inner-end
                                                                                                                                               :children (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                                       pending-tokens)
                                                                                                                                                                        (rest delimiter-ids)
                                                                                                                                                                        loose-children
                                                                                                                                                                        inner-end
                                                                                                                                                                        parent-id)}))
                                                                                                                     sibling-tokens]
                                                                                                :else [result pending-tokens]))]
                                                                  (recur next-tokens
                                                                         next-results
                                                                         next-openers
                                                                         next-sibling-tokens))

                                                                ;; Ambiguous tokens
                                                                (let [[next-results
                                                                       next-tokens] (if (killed? id)
                                                                                      [result pending-tokens]
                                                                                      [(conj sibling-elements
                                                                                             (merge {:start start-idx
                                                                                                     :end end-idx
                                                                                                     :count quantity
                                                                                                     :id id}
                                                                                                    (case id
                                                                                                      :code {:content (get-sub (+ delimiter-length start-idx)
                                                                                                                               (- end-idx delimiter-length))}
                                                                                                      :codeblock (let [[_ lang content] (->> (get-sub (+ start-idx delimiter-length)
                                                                                                                                                     (- end-idx delimiter-length))
                                                                                                                                            (.exec codeblock-re))]
                                                                                                                   {:language (if (nil? lang) default-codeblock-lang lang)
                                                                                                                    :content content})
                                                                                                      :latex {:content (get-sub (+ delimiter-length start-idx)
                                                                                                                                (- end-idx delimiter-length))}
                                                                                       ;; formatting
                                                                                                      {:children-start (+ delimiter-length start-idx)
                                                                                                       :children-end (- end-idx delimiter-length)
                                                                                                       :children (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                               pending-tokens)
                                                                                                                                (rest delimiter-ids)
                                                                                                                                loose-children
                                                                                                                                (- end-idx delimiter-length)
                                                                                                                                id)})))
                                                                                       sibling-tokens])]
                                                                  (recur (cond->> next-up
                                                                           (and (not use-greedy)
                                                                                (>= length (+ thickness delimiter-length)))
                                                                           (cons (-> current-token
                                                                                     (update :length - thickness)
                                                                                     (update :idx + thickness))))
                                                                         next-results
                                                                         ;; openers
                                                                         (cond-> rest-openers
                                                                           ;; split opener into higher level, outer opener if long enough
                                                                           (and (not use-greedy)
                                                                                (>= partner-length (+ thickness delimiter-length)))
                                                                           (conj (update partner :length - thickness)))
                                                                         ;; update pending tokens
                                                                         next-tokens))))

                                                            ;; is a (potential) closer but no partner found
                                                            (recur next-up
                                                                   (if (and (= :close direction) (= :codeblock id))
                                                                     (conj result {:start idx
                                                                                   :end (+ idx length)
                                                                                   :id :code
                                                                                   :children-start (inc idx)
                                                                                   :children-end (dec (+ idx length))
                                                                                   :content (apply str (repeat (- length 2) "`"))})
                                                                     result)
                                                                   (if (= :close direction)
                                                                     openers
                                                                     ;; dual -> add as opener
                                                                     (add-opener openers current-token))
                                                                   pending-tokens))))

                                                      [(if (seq openers)
                                                         (cond-> result
                                                           (= :codeblock id)
                                                           (into (for [o openers]
                                                                   {:start (:idx o)
                                                                    :end (+ (:idx o) (:length o))
                                                                    :id :code
                                                                    :children-start (inc (:idx o))
                                                                    :children-end (dec (+ (:idx o) (:length o)))
                                                                    :content (apply str (repeat (- (:length o) 2) "`"))})))
                                                         result)
                                                       pending-tokens])))))]
                          (loop [pending-rules allowed-ids
                                 all-result existing-elements
                                 all-pending-tokens tokens]
                            (if (and (seq pending-rules) (seq all-pending-tokens))
                              (let [[result next-pending-tokens] (process-token pending-rules all-pending-tokens all-result)]
                                (recur (next pending-rules) result next-pending-tokens))
                              all-result))))
        phase-3 (fn phase-3 [parent start end allowed-children]
                  (let [parent-definition (get element-definitions (:id parent))
                        children (reduce into (map (fn [c]
                                                     (if (:children c)
                                                       (let [child-definition (get element-definitions (:id c))]
                                                         (phase-3 c (:children-start c) (:children-end c)
                                                                  (cond-> (:allowed-children child-definition)
                                                                    (not (:invisible? child-definition)) (clojure.set/intersection allowed-children))))
                                                       [c]))
                                                   (:children parent)))
                        ids (set (keep :id children))
                        killed? (or (when-let [killed-by (:killed-by parent-definition)]
                                        (some killed-by ids))
                                      (when (= :inline (:scope parent-definition))
                                        (seq (filter #(< (:start parent) (:idx %) (:end parent)) (:newline all-tokens)))))]
                    (if killed?
                      children
                      [(assoc parent :children (add-text-nodes (filter #(contains? allowed-children (:id %)) children)
                                                               start end))])))

        hit-tokens (dissoc (select-keys all-tokens (for [[k v] all-tokens :when (seq v)] k))
                           :blockquote :hiccup)
        process-elements (fn process-elements [elements]
                           (for [i elements]
                             (subs string (:start i) (:end i))))
        root-text-elements (fn root-text-elements ([existing-els] (add-text-nodes (process-slice all-tokens rules existing-els (.-length string) :block) 0 (.-length string)))
                             ([] (root-text-elements [])))]
    (if-let [hc (first (:hiccup all-tokens))]
      [{:id :hiccup
        :start 7
        :end (dec (.-length string))
        :children []}]
      (if-let [bq (first (:blockquote all-tokens))]
        [{:id :blockquote
          :start (:length bq)
          :end (dec (.-length string))
          :children (root-text-elements)}]
        (:children (first (let [x (root-text-elements)] (phase-3 {:id :block :children-start 0 :children-end (.-length string) :children x}
                                                                 0 (.-length string) (:allowed-children (get element-definitions :block))))))))))

(defn replace-node [f coll new]
  (let [idxs (keep-indexed #(when (f %2) %1) coll)
        idx (first idxs)]
    (assoc coll idx new)))

(defn replace-in [tree path new]
  (update-in tree path (fn [old] new)))
