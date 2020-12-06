(ns roam-parser.core)

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
                             4))}})

(def style-markers {:italic "__"
                    :bold "**"
                    :highlight "^^"})
(def style-marks {"__" :italic
                    "**" :bold
                    "^^" :highlight })

(defn parse-text [text]
  (let []))

(defn parse-block [block]
  (let [tree {}]
    (if (= "---" block)
      [{:type :hr}]
      (let [tokens (loop [chunks []
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
                             (pop chunks-2))))))]
        (if (.startsWith block ">")
          [{:type :blockquote
            :children tokens}]
          tokens)))))

(def test-str-f "There are some **bugs** in our parser - it__ doesn't handle certain kinds of^^ nesting - and it __is a bit of a pain to extend -- also - it is painful whenever we want t^^o make up more specific syntax - like for queries.")

(def test-str "There are some bugs in our parser - it doesn't handle certain kinds of nesting - and it is a bit of a pain to extend -- also - it is painful whenever we want to make up more specific syntax - like for queries.")

(defn test-1000 []
  (time (doall (for [i (range 1000)] (parse-block test-str-f))))
  nil)

(set! (.-test js/window) test-1000)
(set! (.-parb js/window) parse-block)
