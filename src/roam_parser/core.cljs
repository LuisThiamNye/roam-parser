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
                      :boldClose :close}}
                 {:regex #"(?<!_)(?:(?<italicOpen>(?<!\S)_{2,}(?=\S))|(?<italicClose>(?<=\S)_{2,}(?!\S))|(?<italicDual>(?<=\S)_{2,}(?=\S)))(?!_)"
                  :id "italic"}
                 {:regex #"(?<!\^)(?:(?<highlightOpen>(?<!\S)\^{2,}(?=\S))|(?<highlightClose>(?<=\S)\^{2,}(?!\S))|(?<highlightDual>(?<=\S)\^{2,}(?=\S)))(?!\^)"
                  :id "highlight"}])

(defn probe [x] (.log js/console x) x)


(def inline-re (let [trim-ends #(.slice % 1 -1)]
                 (reduce #(str %1 "|" %2)
                         (map #(trim-ends (str (:regex %)))
                              delimiters))))

(defn parse-inline [string]
  (let [matches (.matchAll string inline-re)
        tokens (for [m matches
                     :let [idx (.-index m)
                           matched-text (first m)
                           group (loop [rest-groups (.entries js/Object (.-groups m))]
                                   (if rest-groups
                                     (let [[name value] (first rest-groups)]
                                       (if value
                                         name
                                         (recur (next rest-groups))))
                                     nil))
                           [element direction] (rest (re-find #"(.*?)([A-Z])[a-z]+$" group))
                           length (count matched-text)]]
                 {:id element
                  :direction (case direction
                               "O" :open
                               "C" :close
                               :dual)
                  :next-idx (+ idx length)
                  :length length
                  :idx idx})
        process-layer (let [add-opener #(update % (:id %2) conj {:idx (:idx %2) :length (:length %2)})]
                        (loop [pending tokens
                               result []
                               openers (merge (map #(hash-map (:id %) []) delimiters))]
                          (if pending
                            (let [{:keys [id direction next-idx length idx] :as current-token} (first pending)]
                              (if (case direction
                                    :open true
                                    :close false
                                    :dual )
                                (recur (next pending)
                                       result
                                       (conj openers current-token))))
                            result)))]
    ))

(defn parse-block [block]
  (let []
    
    markers))

(def test-str-f "There are_ some__ **bug*s**** in our _parser^ - it__ doesn't handle certain kinds of^^ nesting - and it __is ^^^a bi__t of a pain to ex_tend -- also - it *is *painful whenever we want t^__^^^o make up more speci^^fic syntax - like for queries.")

(def test-str "There are some bugs in our parser - it doesn't handle certain kinds of nesting - and it is a bit of a pain to extend -- also - it is painful whenever we want to make up more specific syntax - like for queries.")

(defn test-1 []
  (time (doall (for [i (range 10000)] (parse-block test-str-f))))
  nil)
(defn test-2 []
  (time (doall (for [i (range 10000)] (parse-inline test-str-f))))
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
