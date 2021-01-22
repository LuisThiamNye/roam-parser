(ns roam-parser.builder (:require [clojure.string]
                                  [roam-parser.utils :as utils]
                                  [roam-parser.rules :as rules]
                                  [roam-parser.transformations :as transf]))

(defn probe [x] (.log js/console x) x)

(def str-replace-re (js/RegExp. (str "\\\\(?<escape>" (utils/re-to-str rules/escapable-char-regex) ")")
                                "g"))
(defn escape-str [^string string] (.replace string str-replace-re "$<escape>"))

(defn get-sub
  ([^string string start end no-escape]
   (subs string start end))
  ([^string string start end]
   (-> (subs string start end)
       escape-str)))

(defn get-match [^string string re start end]
  (re-find re (subs string start end)))


;; ---------------============================-----




(defn find-elements [string]
  (let [str-length (count string)
        runs       (volatile! 0)]
    (loop [state {:path   [{:context/id       :context/block
                            :open-idx         0
                            :context/elements []
                            :context/rules    rules/rules}]
                  :idx    0
                  :string string}]
      (vswap! runs inc)
      (if (> @runs 100000)
        (js/console.log "too many recurs")
        (let [idx (:idx state)]
          (if (< idx str-length)
            (recur (transf/process-char state (-> state :path peek :context/rules)) )
            (let [path (:path state)]
              (if (< 1 (count path))
                (recur (transf/fallback-state (peek path)))
                (-> state :path (nth 0) :context/elements)))))))))

;;
;; **** rich comment block ****
;;

(comment
  (find-elements "[[[[a]]d[[b]]]]hhlolol")
  (simple-benchmark []   (find-elements "[[[[a]]d[[b]]]]hhlolol") 10000)



  ;; bracket run length lookahead
  ;; probably better to not as assumption works better
  #_:clj-kondo/ignore
  (let [char-count (count (re-find #"\[*" (subs string idx)))
        n-doubles  (quot char-count 2)
        a-single?  (pos? (mod char-count 2))]
    (loop [n n-doubles
           s state]
      ))


  (simple-benchmark [] {:context/id       :context/alias-square
                        :open-idx         (inc 54)
                        :context/elements []} 10000)
  ;; 7

  (simple-benchmark [] (rand-nth ["a" "b" "c"]) 1000000)
  ;; 40
  (simple-benchmark [  ] (case (rand-nth ["a" "b" "c"]) "b" 5 "c" 8 "a" 3 0) 1000000)
  ;; 45 - 40 = 5
  (simple-benchmark [  ] (case (rand-nth [:a :b :c]) :b 5 :c 8 :a 3 0) 1000000)
  ;; 101 - 40 =60
  (simple-benchmark [x {"b" 5 "c" 8 "a" 3}] (get x "a") 10000)
  ;; 9


  (simple-benchmark [] (= "a" "b") 100000)
  ;; 13
  (simple-benchmark [] (identical? "a" "b") 100000)
  ;; 1
  (simple-benchmark [] (case "a" "b" true false) 100000)
  ;; 1

  (defn some-recur
    ([f]
     (fn [i]
       (if (> i 15)
         "new state"
         (f (inc i))))))

  (def stak (apply comp (take 15 (repeat some-recur))))
  (simple-benchmark[] (comp some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur ) 10000)
  ;; 40

  (def active (stak identity))

  (simple-benchmark [](active 2) 100000)
  ;; 16

  (defn try-smth [i]
    (if (> i 15)
      "new state"
      nil))

  (simple-benchmark []
                    (loop [i 1]
                      (if (nil? (try-smth i))
                        (recur (inc i))
                        "yes")) 100000)
  ;; 16

  (simple-benchmark [] (let [[a b] [{:a :map} true]]
                         [a b]) 100000)
  ;; 23

  (def path [{:a 4} {:b 8} 2 3 4 5 6 83 29234 23 423 4 2 2 2 34029932  49 4 32  2 23 439])

  (simple-benchmark [] (seq path) 100000)
  ;; 9

  (simple-benchmark [] (pos? (count path)) 100000)
  ;; 3
  )
