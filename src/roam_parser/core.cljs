(ns roam-parser.core (:require ["ahocorasick" :as ahc]))

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

(defn od [ block] (loop [chunks []
                    run-length 0
                    idx 0
                    this-chunk {:type :display :styles #{} :children []}
                    string block]
               (let [new-chunks (fn [] (let [last-chunk (if (not= 0 run-length)
                                                         (update this-chunk :children conj (subs block (- idx run-length) (dec idx)))
                                                         this-chunk)]
                                        (if (empty? (:children last-chunk))
                                          chunks
                                          (conj chunks last-chunk))))]
                 (if (seq string)
                   (if-let [matched-style (some #(when (.startsWith string (val %)) (key %)) style-markers)]
                     ;; Change to style
                     (let [next-styles (if (contains? (:styles this-chunk) matched-style)
                                         (disj (:styles this-chunk) matched-style)
                                         (conj (:styles this-chunk) matched-style))]
                       (recur (new-chunks)
                              0
                              (+ idx (count (matched-style style-markers)))
                              {:type :display
                               :styles next-styles
                               :children []}
                              (subs string (count (matched-style style-markers)))))
                     ;; no change
                     (recur chunks
                            (inc run-length)
                            (inc idx)
                            this-chunk
                            (subs string 1)))
                   ;; end of string, return
                   (let [chunks-2 (new-chunks)]
                     (if (or (seq (:children (peek chunks-2))) (empty? chunks))
                       chunks-2
                       (pop chunks-2)))))))

(defn parse-block-old [block]
  (let [tree {}]
    (if (= "---" block)
      [{:type :hr}]
      (let [markers (sort-by :idx (apply concat (for [[_name marker] style-markers]
                                                  (let [marker-length (count marker)
                                                        element-name _name
                                                        limit-type :two-way]
                                                    (loop [occurrences []
                                                           from-idx -1]
                                                      (let  [idx (.indexOf block marker from-idx)
                                                             next-idx (+ idx marker-length)]
                                                        (if (< -1 idx)
                                                          ;; found occurrence
                                                          (recur (conj occurrences {:name element-name
                                                                                    :limit-type limit-type
                                                                                    :next-idx next-idx
                                                                                    :idx idx})
                                                                 next-idx)
                                                          (if (or (even? (count occurrences)) (not= :two-way limit-type))
                                                            occurrences
                                                            (pop occurrences)))))))))
            display-allowed-children #{:page :alias :image :blockref}
            parse-text (loop [rest-markers markers
                              active-type :display
                              active-layers #{}
                              children []
                              nodes []
                              allowed-markers #{:display}
                              terminator nil]
                         (if (seq rest-markers)
                           (let [current-marker (first rest-markers)]
                             (if (= :display active-type)
                               (if-let [matched-style (some #{(:name current-marker)} (keys style-markers))]
                                 ;; Change to style
                                 (let [next-layers (if (contains? active-layers matched-style)
                                                     (disj active-layers matched-style)
                                                     (conj active-layers matched-style))]
                                   (recur (rest markers)
                                          active-type
                                          active-layers
                                          next-layers
                                          []
                                          (conj nodes {:type :display
                                                       :layers active-layers
                                                       :children children})
                                          nil))
                                 ;; other marker found; need to switch from display
                                 (if (some #{(:name current-marker)} allowed-markers)
                                   (and (= :end (:limit-type current-marker)) ())
                                   (recur (rest markers)
                                          active-type
                                          active-layers
                                          children
                                          nodes
                                          allowed-markers
                                          nil))))
                             )
                           (conj nodes {:type active-type
                                        :layers active-layers
                                        :children children})))]
        (if (.startsWith block ">")
          [{:type :blockquote
            :children markers}]
          parse-text)))))


(defn parse-inline-ahc [string]
  (let [res (.search @(atom (ahc. #js ["**" "__" "^^"]))
                     string)]
    (for [[idx matches] res]
      {:name (get style-marks (last matches))
       :limit-type :two-way
       :next-idx (- idx (count (last matches)))
       :idx idx})))

(defn parse-inline [string]
  (let [res (.matchAll string #"\\*\\*|\\^\\^|__" )]
    (for [m res]
      (let [idx (.-index m)]
        {:name (first m)
         :limit-type :two-way
         :next-idx (+ idx (count (first m)))
         :idx idx}))))

(defn parse-block [block]
  (let [markers (sort-by :idx (apply concat (for [[_name marker] style-markers]
                                              (let [marker-length (count marker)
                                                    element-name _name
                                                    limit-type :two-way]
                                                (loop [occurrences []
                                                       from-idx -1]
                                                  (let  [idx (.indexOf block marker from-idx)
                                                         next-idx (+ idx marker-length)]
                                                    (if (< -1 idx)
                                                      ;; found occurrence
                                                      (recur (conj occurrences {:name element-name
                                                                                :limit-type limit-type
                                                                                :next-idx next-idx
                                                                                :idx idx})
                                                             next-idx)
                                                      (if (or (even? (count occurrences)) (not= :two-way limit-type))
                                                        occurrences
                                                        (pop occurrences)))))))))]
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
