(require '[ clojure.string :as cs ])
(require '[roam-parser.tokens.core])
(require '[roam-parser.dev.sample])

(def s "this is some test text taht could perthaps be a block or some thingsj dfj dlfhsiola hl dlkjfha lsdkfhla kdflajkndf ldnfdf kasjdf ")

(def sq (seq s))

;; 8
(simple-benchmark [] (subs s 20 40) 100000)

;; 712
(simple-benchmark [] (cs/join (->> sq (drop 20) (take 20))) 100000)

;; 743
(simple-benchmark [] (apply str (->> sq (drop 20) (take 20))) 100000)

(def part (doall (->> sq (drop 20) (take 20))))

;; 366
(simple-benchmark [] (cs/join part) 100000)

(simple-benchmark [] (nth s 20) 100000)
;; 10

(simple-benchmark [] (next sq) 100000)
;; 6
(simple-benchmark [] (first sq) 100000)
;; 5
(simple-benchmark [] (first (next sq)) 100000)
;;12

(simple-benchmark [] (loop [coll sq]
                       (if (nil? coll)
                         nil
                         (do (first coll)
                             (recur (next coll))))) 1000)


(simple-benchmark [] (loop [x 0] (if (neg? x) (case (nth s 20) "a" 5 "b" 6 "c" 7 33) (recur (dec x)))) 100000)
;; 10

;; 14
(simple-benchmark [] (.exec (js/RegExp. roam-parser.tokens.core/inline-re "gm" ) s) 1000)

(def lines (cs/split-lines roam-parser.dev.sample/text))

(/ (transduce (map count) + lines) (count lines))
;; => 77.21255612167684

(simple-benchmark [] (= :bobert :bobette) 100000)
;; 16
(simple-benchmark [] (#{ :bobert } :bobette) 100000)
;; 17

(simple-benchmark [] (= :bobert :bobert) 100000)
;; 17
(simple-benchmark [] (#{ :bobert } :bobert) 100000)
;; 20
