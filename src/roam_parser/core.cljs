(ns roam-parser.core (:require [clojure.set]))

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

(def rules [:hr :codeblock :code :curly :square :round :latex :highlight :bold :italic :url :attribute :tag])

(def style-el-definition {:allowed-children #{:parenthetical :code :page :alias
                                              :bold :italic :highlight}
                          :killed-by #{:image :render :latex}
                          :scope :inline})
(def element-definitions {:block {:allowed-children #{:blockquote :hr :codeblock :code :render :page :bracket-tag
                                                      :alias :image :parenthetical :latex :bold :italic :highlight
                                                      :url :attribute :tag}}
                          :blockquote {:allowed-children #{:page :render :image :bold :highlight :italic
                                                           :alias :parenthetical :hr :code :url :latex
                                                           :bracket-tag :tag :block-ref}}
                          :codeblock {}
                          :code {:killed-by #{:codeblock}}
                          :render {:allowed-children #{:page :block-ref}
                                   :killed-by #{:codeblock
                                                :code}
                                   :delimiters #{:curly}}
                          :page {:allowed-children #{:page :bold :highlight :italic}
                                 :delimiters #{:square}
                                 :scope :inline}
                          :bracket-tag {:allowed-children #{:page :bold :highlight :italic}
                                        :delimiters #{:square}
                                        :scope :inline}
                          :alias {:allowed-children #{:latex :code :image
                                                      :bold :highlight :italic}
                                  :delimiters #{:square :round}}
                          :image {:allowed-children #{:bold :highlight :italic
                                                      :image}
                                  :delimiters #{:square :round}}
                          ;; dummy element; never created
                          :alias-destination {:allowed-children #{:page :block-ref}
                                              :killed-by #{:alias}
                                              :scope :inline}
                          :parenthetical {:allowed-children #{:code :render :url :latex
                                                              :bold :highlight :italic
                                                              :page :alias :parenthetical :image
                                                              :bracket-tag :tag :block-ref}
                                          :killed-by #{:codeblock}
                                          :delimiters #{:round}}
                          :latex {:killed-by #{:codeblock}}
                          :bold style-el-definition :italic style-el-definition :highlight style-el-definition

                          ;; singles

                          :hr {}
                          :url {}
                          :attribute {:allowed-children #{:bold :italic :highlight}
                                      :killed-by #{:url :latex :block-ref :parenthetical :image :alias :bracket-tag :render :code}}
                          ;; tags cannot overlap with other delimiters due to regex
                          :tag {}})

(def delimiters [{:id :hiccup
                  :regex #"(?<!\n)^:hiccup(?<hiccup>(?:.|\n)*)"
                  :length 7}
                 {:id :blockquote
                  :regex #"(?<!\n)^(?<blockquot>>|\[\[>\]\]|#\[\[>\]\])(?=.)"}
                 {:id :hr
                  :flags #{:single}
                  :regex #"^---$"
                  :length 3}
                 {:id :codeblock
                  :flags #{:greedy}
                  :regex #"(?<!`)(?:(?<codeblockDual>`{3}(?!.*?`{3}))|(?<codeblockClose>`{3,}))(?!`)"
                  :length 3}
                 {:id :code
                  :flags #{:greedy}
                  :regex #"`+"
                  :length 1}
                 {:id :curly
                  :flags #{:bracket}
                  :regex #"\{+|\}+"
                  :length 1}
                 {:id :attribute
                  :flags #{:single}
                  :regex #"(?<=\S)::"}
                 {:id :latex
                  :regex #"(?<!\$)(?:(?<latexOpen>(?<!\S)\${2}(?=\S))|(?<latexClose>(?<=\S)\${2}(?!\S))|(?<latexDual>(?<=\S)\${2}(?=\S)))(?!\$)"
                  :length 2}
                 {:id :square
                  :flags #{:bracket}
                  :regex #"\[+|\]+"
                  :length 1}
                 {:id :round
                  :flags #{:bracket}
                  :regex #"\(+|\)+"
                  :length 1}
                 {:id :bold
                  :flags #{:greedy}
                  :regex #"(?<!\*)(?:(?<boldOpen>(?<!\S)\*{2,}(?=\S))|(?<boldClose>(?<=\S)\*{2,}(?!\S))|(?<boldDual>(?<=\S)\*{2,}(?=\S)))(?!\*)"
                  :length 2}
                 {:id :italic
                  :flags #{:greedy}
                  :regex #"(?<!_)(?:(?<italicOpen>(?<!\S)_{2,}(?=\S))|(?<italicClose>(?<=\S)_{2,}(?!\S))|(?<italicDual>(?<=\S)_{2,}(?=\S)))(?!_)"
                  :length 2}
                 {:id :highlight
                  :flags #{:greedy}
                  :regex #"(?<!\^)(?:(?<highlightOpen>(?<!\S)\^{2,}(?=\S))|(?<highlightClose>(?<=\S)\^{2,}(?!\S))|(?<highlightDual>(?<=\S)\^{2,}(?=\S)))(?!\^)"
                  :length 2}
                 {:id :tag
                  :flags #{:single}
                  :regex #"(?<=[ \(\[:]|^)#(?<tag>[\w-_/@:]+(?<!::)(?!:(?!:)))"}
                 {:id :hash
                  :regex #"#(?=\[)"
                  :length 1}
                 {:id :url
                  :flags #{:single}
                  :regex #"(?<![\w\.-])(?<url>https?://[a-zA-Z\d-\.]+\.[a-zA-Z\d-]{2,}|www\.[a-zA-Z\d-\.]+\.[a-zA-Z\d-]{2,})"}
                 {:id :!
                  :regex #"!(?=\[)"
                  :length 1}
                 {:id :newline
                  :regex #"\n+"}])

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

(defn symbol-data-from-char [first-char]
  (case first-char
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
    "`" {:id :code}
    "-" {:id :hr}
    "#" {:id :hash}
    "!" {:id :!}
    ":" {:id :attribute}
    "\n" {:id :newline}
    (.log js/console "Found an unknown symbol")))

(def inline-re (js/RegExp. (let [trim-ends #(.slice % 1 -1)]
                             (reduce #(str %1 "|" %2)
                                     (map #(trim-ends (str (:regex %)))
                                          delimiters)))
                           ;; modifier flags
                           "gm"))


(defn parse-inline [string]
  (let [matches (.matchAll string inline-re)
        ;; map of delimiters and their occurrences
        all-tokens (loop [match-stack (for [m matches]
                                        {:idx (.-index m)
                                         :group (loop [groups (.entries js/Object (.-groups m))]
                                                  (if groups
                                                    (let [[name value] (first groups)]
                                                      (if value
                                                        [name value]
                                                        (recur (next groups))))
                                                    ;; does not match any group
                                                    nil))
                                         :text (first m)})
                          output (reduce #(assoc % (:id %2) [])
                                         {} delimiters)]
                     (if (seq match-stack)
                       (let [m (first match-stack)
                             length (count (:text m))
                             idx (:idx m)
                             group-name (first (:group m))
                             symbol-data (if group-name
                                           (assoc (symbol-data-from-group group-name)
                                                  :text (last (:group m)))
                                           (symbol-data-from-char (first (:text m))))]
                         (recur (next match-stack)
                                (update output (:id symbol-data)
                                        conj (merge symbol-data
                                                    {:length length
                                                     :idx idx}))))
                       output))
        filter-tokens (fn [f tks] (loop [x tks
                                         ks (keys tks)]
                                    (if (seq ks)
                                      (recur (update x (first ks) (partial filter f))
                                             (next ks))
                                      x)))
        find-token (fn [tks id idx] (first (filter #(= (:idx %) idx) (get tks id))))
        add-opener (fn add-opener
                     ([openers new] (conj openers {:idx (:idx new) :length (:length new)}))
                     ([openers new tag] (conj openers {:idx (:idx new) :length (:length new) :tag tag})))
        escape-str (fn [string] string)
        get-sub (fn ([start end] (subs string start end))
                  ([start end no-escape] (subs string start end)))
        get-match (fn [re start end]
                    (re-find re (subs string start end)))
        add-text-nodes (fn add-text-nodes [elements start end]
                         (let [add-text-node (fn [coll start end]
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
                              process-token (fn [delimiter-ids all-pending-tokens all-result]
                                              (let [id (first delimiter-ids)
                                                    delimiter-data (first (filter #(= (:id %) id) delimiters))
                                                    use-greedy (contains? (:flags delimiter-data) :greedy)
                                                    is-single (contains? (:flags delimiter-data) :single)
                                                    is-bracket (contains? (:flags delimiter-data) :bracket)
                                                    delimiter-length (:length delimiter-data)]
                                                (if is-single

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
                                                                                          (if (or (.startsWith page-name " ") (> -1 (.indexOf page-name "\n")))
                                                                                            result
                                                                                            (conj sibling-tokens {:start 0
                                                                                                                  :end (+ (:idx current-token) (:length current-token))
                                                                                                                  :id id
                                                                                                                  :page page-name
                                                                                                          ;; no need to process children further as it cannot contain anything of a lower priority anyway
                                                                                                                  :children children})))
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
                                                                                                    :end (+ (:idx current-token) (count url-string))
                                                                                                    :id :url
                                                                                                    :url url-string}]
                                                                                   [(conj result new-element)
                                                                                    (filter-tokens pending-tokens #(not (< (:start new-element) (:idx %) (:end new-element))))])

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
                                                                 (if (and (= :curly id) (empty? openers) (= length 1))
                                                                   openers
                                                                   (add-opener openers current-token (:tag current-token)))
                                                                 pending-tokens)

                                                          ;; found potential closer
                                                          (if-let [partner (last openers)]
                                                            ;; found a matching pair, add to results
                                                            (let [quantity (if use-greedy 1
                                                                               (.floor js/Math (/ (min (:length partner) length) delimiter-length)))
                                                                  thickness (* quantity delimiter-length)
                                                                  start-idx (if use-greedy
                                                                              (:idx partner)
                                                                              (- (+ (:idx partner) (:length partner)) thickness))
                                                                  is-greedy use-greedy
                                                                  partner-length (:length partner)
                                                                  end-idx (if is-greedy
                                                                            (+ idx length)
                                                                            (+ idx thickness))
                                                                  loose-children (filter #(< start-idx (:start %) idx) result)
                                                                  is-killed (fn [element-id]
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
                                                              (if is-bracket
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
                                                                                                              (let [squares (first (filter #(= (:end %) start-idx) result))
                                                                                                                    element-id (:tag partner)
                                                                                                                    [dest-type
                                                                                                                     dest] (let [children-start inner-start
                                                                                                                                 children-end inner-end]
                                                                                                                             (cond
                                                                                                                               (and (= quantity 3)
                                                                                                                                    not-empty) [:block-ref
                                                                                                                                                (get-sub children-start children-end :no-escape)]
                                                                                                                               (= quantity 1) (let [children (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                                           pending-tokens)
                                                                                                                                                                            (rest delimiter-ids)
                                                                                                                                                                            loose-children
                                                                                                                                                                            children-end
                                                                                                                                                                            :alias-destination)
                                                                                                                                                    first-child (first children)]
                                                                                                                                                (if (and (= 1 (count children))
                                                                                                                                                         (= :page (:id first-child))
                                                                                                                                                         (= children-start (:start first-child))
                                                                                                                                                         (= children-end (:end first-child)))
                                                                                                                                                  [:page
                                                                                                                                                   (:page first-child)]
                                                                                                                                                  [:url
                                                                                                                                                   (get-sub children-start children-end)]))))]

                                                                                                                (if (and dest-type (not (is-killed element-id)))
                                                                                                                  [(conj (remove #(= (:start %) (:start squares)) sibling-elements)
                                                                                                                         {:start (cond-> (:start squares)
                                                                                                                                   (= :image element-id) dec)
                                                                                                                          :divider-idx (dec (:end squares))
                                                                                                                          :end end-idx
                                                                                                                          :id element-id
                                                                                                                          :children (:children squares)
                                                                                                                          :destination-type dest-type
                                                                                                                          :destination dest})
                                                                                                                   sibling-tokens]
                                                                                                                  [result
                                                                                                                   pending-tokens])))]
                                                                                              (if not-empty
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
                                                                                                                         (if (is-killed alias-id)
                                                                                                                           [result
                                                                                                                            pending-tokens]
                                                                                                                           [(conj sibling-elements {:end end-idx
                                                                                                                                                    :count 1
                                                                                                                                                    :start start-idx
                                                                                                                                                    :id id
                                                                                                                                                    :children-start (+ start-idx delimiter-length)
                                                                                                                                                    :children-end (- end-idx delimiter-length)
                                                                                                                                                    :children (add-text-nodes (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                                                            pending-tokens)
                                                                                                                                                                                             (rest delimiter-ids)
                                                                                                                                                                                             loose-children
                                                                                                                                                                                             (- end-idx delimiter-length)
                                                                                                                                                                                             alias-id)
                                                                                                                                                                              (+ start-idx delimiter-length)
                                                                                                                                                                              (- end-idx delimiter-length))})
                                                                                                     ;; mark opening round as potential alias
                                                                                                                            (assoc sibling-tokens
                                                                                                                                   :round (sort-by :idx (conj
                                                                                                                                                         (remove #(= (:idx %) (:idx following-round)) (:round sibling-tokens))
                                                                                                                                                         (assoc following-round :tag alias-id))))]))
                                                                                                ;; this is a page [[ ]]
                                                                                                                       (let [page-id (if (find-token pending-tokens :hash (dec start-idx))
                                                                                                                                       :bracket-tag
                                                                                                                                       :page)]
                                                                                                                         (if (is-killed page-id)
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
                                                                                                                                                    :children (add-text-nodes (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                                                            pending-tokens)
                                                                                                                                                                                             (rest delimiter-ids)
                                                                                                                                                                                             loose-children
                                                                                                                                                                                             inner-end
                                                                                                                                                                                             page-id)
                                                                                                                                                                              inner-start
                                                                                                                                                                              inner-end)})
                                                                                                                            sibling-tokens])))
                                                                                              ;; this is a single pair of brackets to discard
                                                                                                                     [result
                                                                                                                      pending-tokens]))
                                                                                                  (test-if-alias) (try-alias)
                                                                                                  (= id :round) (if (and (> thickness delimiter-length) (not (is-killed :parenthetical)))
                                                                                                                  [(conj sibling-elements {:start start-idx
                                                                                                                                           :end end-idx
                                                                                                                                           :count 1
                                                                                                                                           :id :parenthetical
                                                                                                                                           :children-start inner-start
                                                                                                                                           :children-end inner-end
                                                                                                                                           :children (add-text-nodes (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                                                   pending-tokens)
                                                                                                                                                                                    (rest delimiter-ids)
                                                                                                                                                                                    loose-children
                                                                                                                                                                                    inner-end
                                                                                                                                                                                    :parenthetical)
                                                                                                                                                                     inner-start
                                                                                                                                                                     inner-end)})
                                                                                                                   sibling-tokens]
                                                                                           ;;  a normal pair of ( ) brackets, discard and surface children as an isolated bunch
                                                                                                                  [(process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                 pending-tokens)
                                                                                                                                  (rest delimiter-ids)
                                                                                                                                  result
                                                                                                                                  inner-end
                                                                                                                                  parent-id)
                                                                                                                   sibling-tokens])
                                                                                                  (= id :curly) [(conj sibling-elements (if (and (= 1 (count openers))
                                                                                                                                                 (>= thickness 2)
                                                                                                                                                 (not (is-killed :render)))
                                                                                                                                          {:start (- (+ (:idx partner) (:length partner)) 2)
                                                                                                                                           :end (+ idx 2)
                                                                                                                                           :count 1
                                                                                                                                           :id :render
                                                                                                                                           :children-start inner-start
                                                                                                                                           :children-end inner-end
                                                                                                                                           :children (add-text-nodes (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                                                   pending-tokens)
                                                                                                                                                                                    (rest delimiter-ids)
                                                                                                                                                                                    loose-children
                                                                                                                                                                                    inner-end
                                                                                                                                                                                    :render)
                                                                                                                                                                     inner-start inner-end)}
                                                                                                                                          {:start start-idx
                                                                                                                                           :end end-idx
                                                                                                                                           :count quantity
                                                                                                                                           :id id
                                                                                                                                           :children-start inner-start
                                                                                                                                           :children-end inner-end
                                                                                                                                           :children (add-text-nodes (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                                                                   pending-tokens)
                                                                                                                                                                                    (rest delimiter-ids)
                                                                                                                                                                                    loose-children
                                                                                                                                                                                    inner-end
                                                                                                                                                                                    parent-id)
                                                                                                                                                                     inner-start inner-end)}))
                                                                                                                 sibling-tokens])
                                                                                                (if (test-if-alias)
                                                                                                  (try-alias)
                                                                                                  [result pending-tokens])))]
                                                                  (recur next-tokens
                                                                         next-results
                                                                         next-openers
                                                                         next-sibling-tokens))

                                                                ;; Ambiguous tokens
                                                                (let [[next-results
                                                                       next-tokens] (if (is-killed id)
                                                                                      [result pending-tokens]
                                                                                      [(conj sibling-elements
                                                                                             (merge {:start start-idx
                                                                                                     :end end-idx
                                                                                                     :count quantity
                                                                                                     :id id}
                                                                                                    (case id
                                                                                                      :code {:content (get-sub (+ delimiter-length start-idx)
                                                                                                                               (- end-idx delimiter-length))}
                                                                                                      :codeblock {:content (get-sub (+ start-idx delimiter-length)
                                                                                                                                    (- end-idx delimiter-length))}
                                                                                                      :latex {:content (get-sub (+ delimiter-length start-idx)
                                                                                                                                (- end-idx delimiter-length))}
                                                                                       ;; formatting
                                                                                                      {:children-start (+ delimiter-length start-idx)
                                                                                                       :children-end (- end-idx delimiter-length)
                                                                                                       :children (add-text-nodes (process-slice (filter-tokens #(< (:idx partner) (:idx %) idx)
                                                                                                                                                               pending-tokens)
                                                                                                                                                (rest delimiter-ids)
                                                                                                                                                loose-children
                                                                                                                                                (- end-idx delimiter-length)
                                                                                                                                                id)
                                                                                                                                 (+ delimiter-length start-idx)
                                                                                                                                 (- end-idx delimiter-length))})))
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
                                                                                   :children [{:id :text
                                                                                               :start (inc idx)
                                                                                               :end (dec (+ idx length))
                                                                                               :content (apply str (repeat (- length 2) "`"))}]})
                                                                     result)
                                                                   (if (= :close direction)
                                                                     openers
                                                                     ;; dual -> add as opener
                                                                     (add-opener openers current-token))
                                                                   pending-tokens))))

                                                      [(if (> (count openers) 0)
                                                         (cond-> result
                                                           (= :codeblock id)
                                                           (concat (for [o openers]
                                                                     {:start (:idx o)
                                                                      :end (+ (:idx o) (:length o))
                                                                      :id :code
                                                                      :children [{:id :text
                                                                                  :start (inc (:idx o))
                                                                                  :end (dec (+ (:idx o) (:length o)))
                                                                                  :content (apply str (repeat (- (:length o) 2) "`"))}]})))
                                                         result)
                                                       pending-tokens])))))]
                          (loop [pending-rules allowed-ids
                                 all-result existing-elements
                                 all-pending-tokens tokens]
                            (if (and (seq pending-rules) (seq all-pending-tokens))
                              (let [[result next-pending-tokens] (process-token pending-rules all-pending-tokens all-result)]
                                (recur (next pending-rules) result next-pending-tokens))
                              ;; filter out disallowed children
                              (filter #(some #{(:id %) :text} (:allowed-children (get element-definitions parent-id)))
                                      all-result)))))
        hit-tokens (dissoc (select-keys all-tokens (for [[k v] all-tokens :when (seq v)] k))
                           :blockquote :hiccup)
        process-elements (fn [elements]
                           (for [i elements]
                             (subs string (:start i) (:end i))))
        root-text-elements (fn root-text-elements ([existing-els] (add-text-nodes (process-slice all-tokens rules existing-els (count string) :block) 0 (count string)))
                             ([] (root-text-elements [])))]
    (if-let [hc (first (:hiccup all-tokens))]
      [{:id :hiccup
        :start 7
        :end (dec (count string))
        :children []}]
      (if-let [bq (first (:blockquote all-tokens))]
        [{:id :blockquote
          :start (:length bq)
          :end (dec (count string))
          :children (root-text-elements)}]
        (root-text-elements)))))


(defn replace-node [f coll new]
  (let [idxs (keep-indexed #(when (f %2) %1) coll)
        idx (first idxs)]
    (assoc coll idx new)))

(defn replace-in [tree path new]
  (update-in tree path (fn [old] new)))
