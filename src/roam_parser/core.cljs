(ns roam-parser.core )

(defn j-time
  ( [name x]
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
                    "^^" :highlight })

(defn parse-text [text]
  (let []))


(def delimiters [{:regex #"(?<!\*)(?:(?<boldOpen>(?<!\S)\*{2,}(?=\S))|(?<boldClose>(?<=\S)\*{2,}(?!\S))|(?<boldDual>(?<=\S)\*{2,}(?=\S)))(?!\*)"
                  :id "bold"
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
                  :id "italic"
                  :length 2}
                 {:regex #"(?<!\^)(?:(?<highlightOpen>(?<!\S)\^{2,}(?=\S))|(?<highlightClose>(?<=\S)\^{2,}(?!\S))|(?<highlightDual>(?<=\S)\^{2,}(?=\S)))(?!\^)"
                  :id "highlight"
                  :length 2}
                 {:id "round"
                  :regex #"\(+|\)+"
                  :length 1}])

(defn probe [x] (.log js/console x) x)


(def inline-re (let [trim-ends #(.slice % 1 -1)]
                 (reduce #(str %1 "|" %2)
                         (map #(trim-ends (str (:regex %)))
                              delimiters))))

(defn parse-inline [string]
  (let [matches (.matchAll string inline-re)
        all-tokens (loop [match-stack (for [m matches]
                                        {:idx (.-index m)
                                         :group (loop [rest-groups (.entries js/Object (.-groups m))]
                                                  (if rest-groups
                                                    (let [[name value] (first rest-groups)]
                                                      (if value
                                                        name
                                                        (recur (next rest-groups))))
                                                    nil))
                                         :text (first m)})
                          tbid (reduce #(assoc % (:id %2) [])
                                       {} delimiters)]
                     (if (seq match-stack)
                       (let [m (first match-stack)
                             length (count (:text m))
                             idx (:idx m)
                             type (if (:group m)
                                    (let [[element direction] (rest (re-find #"(.*?)([A-Z])[a-z]+$" (:group m)))]
                                      {:id element
                                       :direction (case direction
                                                    "O" :open
                                                    "C" :close
                                                    :dual)})
                                    (case (first (:text m))
                                      "(" {:id "round"
                                           :direction :open}
                                      ")" {:id "round"
                                           :direction :close}
                                      "[" {:id "square"
                                           :direction :open}
                                      "]" {:id "square"
                                           :direction :close}
                                      "unknown"))]
                         (recur (next match-stack)
                                (update tbid (:id type)
                                        conj (merge type
                                                    {:next-idx (+ idx length)
                                                     :length length
                                                     :idx idx}))))
                       tbid))
        add-opener #(conj % {:idx (:idx %2) :length (:length %2)})
        process-layer (fn process-layer [tokens]
                        (let [process-token (fn [id all-pending-tokens all-result use-greedy]
                                              (loop [ts (get all-pending-tokens id)
                                                     result all-result
                                                     openers []
                                                     pending-tokens all-pending-tokens]
                                                (if (and (seq pending-tokens) (seq ts))
                                                  (if-let [current-token (first ts)]
                                                    (let [{:keys [id direction next-idx length idx]} current-token
                                                          next-up (next ts)
                                                          delimiter-length (:length (first (filter #(= (:id %) id) delimiters )))]
                                                      (if (= :open direction)
                                                        (recur  (cond->> next-up
                                                                     (and use-greedy (> length (* 2 delimiter-length))) ;; is long enough to have content
                                                                     (cons (-> current-token
                                                                               (update :idx + delimiter-length)
                                                                               (update :length - delimiter-length))))
                                                                result
                                                                (add-opener openers current-token)
                                                                pending-tokens)
                                                        ;; found potential closer
                                                        (if-let [partner (last openers)]
                                                          ;; found a matching pair, add to results
                                                          (let [start-idx (+ (:idx partner) delimiter-length)
                                                                is-greedy (and use-greedy)
                                                                end-idx (if is-greedy
                                                                          (+ idx (- length delimiter-length))
                                                                          idx)
                                                                children (filter #(<= start-idx (:start %) idx) result)
                                                                next-result (if (seq children)
                                                                              (filter #(not (<= start-idx (:start %) idx)) result)
                                                                              result)
                                                                filter-tokens (fn [f] (loop [x pending-tokens
                                                                                            ks (keys pending-tokens)]
                                                                                       (if (seq ks)
                                                                                         (recur (update x (first ks) (partial filter f) )
                                                                                                (next ks))
                                                                                         x)))]
                                                            (recur  next-up
                                                                    (conj next-result {:start start-idx
                                                                                       :end end-idx
                                                                                       :id id
                                                                                       :children (concat (process-layer
                                                                                                          (filter-tokens #(< (:idx partner) (:idx %) idx)))
                                                                                                         children)})
                                                                    (pop openers)
                                                                    ;; update pending tokens
                                                                    (filter-tokens #(not (<= (:idx partner) (:idx %) idx)))))
                                                          ;; no partner found
                                                          (if (= :close direction)
                                                            ;; discard
                                                            (recur  next-up
                                                                    result
                                                                    openers
                                                                    pending-tokens)
                                                            ;; dual -> add as opener
                                                            (recur (cond->> next-up
                                                                     (and (> length (* 2 delimiter-length)))
                                                                     (cons (-> current-token
                                                                               (update :idx + delimiter-length)
                                                                               (update :length - delimiter-length))))
                                                                    result
                                                                    (add-opener openers current-token)
                                                                    pending-tokens)))))
                                                    (recur  (next ts)
                                                            result
                                                            openers
                                                            pending-tokens))
                                                  [result pending-tokens])))]
                          (loop [ids (keys tokens)
                                 all-result []
                                 all-pending-tokens tokens]
                            (if (and (seq ids) (seq all-pending-tokens)) 
                              (let [id (first ids)
                                    [result next-pending-tokens] (process-token id all-pending-tokens all-result true)]
                                (recur (next ids) result next-pending-tokens))
                              all-result))))]
    (process-layer all-tokens)))

(defn parse-block [block]
  (let []
    
    nil))

(def test-str-f "There are_ some__ **bug*s**** in our _parser^ - it__ doesn't handle certain kinds of^^ nesting - and it __is ^^^a bi__t of a pain to ex_tend -- also - it *is *painful whenever we want t^__^^^o make up more speci^^fic syntax - like for queries.")

(def test-str "There are some bugs in our parser - it doesn't handle certain kinds of nesting - and it is a bit of a pain to extend -- also - it is painful whenever we want to make up more specific syntax - like for queries.")

(defn test-1 []
  (time (doall (for [i (range 1000)] (parse-block test-str-f))))
  nil)
(defn test-2 []
  (time (doall (for [i (range 1000)] (parse-inline test-str-f))))
  nil)
(defn one-test []
  (.log js/console (time (parse-block test-str-f))))

(set! (.-test js/window) test-1)
(set! (.-test2 js/window) test-2)
(set! (.-parb js/window) parse-block)
(set! (.-parc js/window) parse-inline)
(set! (.-one js/window) one-test)

(defn jim []
  (time (for [i (range 10000)] (if (= "highlight" "highlight") true false))))
