(ns roam-parser.core)

(def markers {:italic {}})

(def style-markers {:italic "__"
                    :bold "**"
                    :highlight "^^"})

(defn parse-text [text]
  (let []))

(defn parse-block [block]
  (let [tree {}]
    {:blockquote (.startsWith block ">")}
    (loop [chunks []
           run-length 0
           idx 0
           this-chunk {:styles '() :children []}
           string block]
      (let [new-chunks (fn [] (let [last-chunk (if (not= 0 run-length)
                                                (update this-chunk :children conj (.substr block (- idx run-length) run-length))
                                                this-chunk)]
                               (if (empty? (:children last-chunk))
                                 chunks
                                 (conj chunks last-chunk))))]
        (if (seq string)
          (if-let [matched-style (some #(when (.startsWith string (val %)) (key %)) style-markers)]
            ;; Change to style
            (let [next-styles (if (some #{matched-style} (:styles this-chunk))
                                (remove #{matched-style} (:styles this-chunk))
                                (conj (:styles this-chunk) matched-style))]
              (recur (new-chunks)
                     0
                     (+ idx (count (matched-style style-markers)))
                     {:styles next-styles
                      :children []}
                     (subs string (count (matched-style style-markers)))))
            ;; no change
            (recur chunks
                   (inc run-length)
                   (inc idx)
                   this-chunk
                   (subs string 1)))
          (let [chunks-2 (new-chunks)]
            (if (empty? (:children (peek chunks-2)))
              (pop chunks-2)
              chunks-2)))))))

;; 668
(def test-str-f "There are some **bugs** in our parser - it__ doesn't handle certain kinds of^^ nesting - and it __is a bit of a pain to extend -- also - it is painful whenever we want t^^o make up more specific syntax - like for queries.")

;; 586
(def test-str "There are some bugs in our parser - it doesn't handle certain kinds of nesting - and it is a bit of a pain to extend -- also - it is painful whenever we want to make up more specific syntax - like for queries.")

(defn test-1000 []
  (time (doall (for [i (range 1000)] (parse-block test-str))))
  nil)
