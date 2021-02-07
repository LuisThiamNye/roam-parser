(ns roam-parser.builder (:require [clojure.string]
                                  [taoensso.timbre :as t]
                                  [roam-parser.rules :refer [rules]]
                                  [roam-parser.utils :as utils]
                                  [roam-parser.transformations :as transf]
                                  [roam-parser.state :as state :refer [initial-state]]))

(defn probe [x] (.log js/console x) x)



(defn get-match [^string string re start end]
  (re-find re (subs string start end)))


;; ---------------============================-----




(defn find-elements [string]
  (let [str-length (count string)
        runs       (volatile! 0)]
    (loop [state (initial-state string rules)]
      (vswap! runs inc)
      (if (> @runs 10000)
        (js/console.log "too many recurs:" string)
        (let [idx (:idx state)]
          (if (< idx str-length)
            (recur (transf/process-char state (-> state ::state/path peek :context/rules)))
            ;; end of block
            (let [path (:roam-parser.state/path state)]
              (if (< 1 (count path))
                (do (t/debug "UNCLOSED AT EOL" (-> state ::state/path peek))
                    (recur (transf/fallback-from-last state)))
                (-> state ::state/path (nth 0) :context/elements
                    (transf/conj-text-el string (-> state ::state/path (nth 0)) str-length))))))))))

;;
;; **** rich comment block ****
;;

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  (simple-benchmark []   (find-elements "[[[[a]]d[[b]]]]hhlolol") 10000)

  (find-elements "[[[[a]]d[[b]]]]hhlolol")


  ;; bracket run length lookahead
  ;; probably better to not as assumption works better


  #_:clj-kondo/ignore
  (let [char-count (count (re-find #"\[*" (subs string idx)))
        n-doubles  (quot char-count 2)
        a-single?  (pos? (mod char-count 2))]
    (loop [n n-doubles
           s state]))

  (simple-benchmark [] {:context/id       :context.id/alias-square
                        :context/open-idx         (inc 54)
                        :context/elements []} 10000)
  ;; 7

  (simple-benchmark [] (rand-nth ["a" "b" "c"]) 1000000)
  ;; 40
  (simple-benchmark [] (case (rand-nth ["a" "b" "c"]) "b" 5 "c" 8 "a" 3 0) 1000000)
  ;; 45 - 40 = 5
  (simple-benchmark [] (case (rand-nth [:a :b :c]) :b 5 :c 8 :a 3 0) 1000000)
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
  (simple-benchmark [] (comp some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur some-recur) 10000)
  ;; 40

  (def active (stak identity))

  (simple-benchmark [] (active 2) 100000)
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

  (def str-ex "the somethign that happened the other day is reallly troubling me and my faamilay ow aiohofpdd")
  (simple-benchmark [] (conj [] {:x (subs str-ex 10 30)}) (* 1000 7))
  ;; 7

  ;; additional time going back over element coll for text nodes
  (def ex-el [{} {} {} {} {} {} {}])
  (simple-benchmark [] (loop [i 6]
                         (if (neg? i)
                           nil
                           (do
                             (nth ex-el i)
                             (recur (dec i))))) 1000)
  ;; 4

  (def re (js/RegExp #"\w*"))
  (simple-benchmark [] (re-find re str-ex) 10000)
  ;; 4

  ;;;;;;;;;;;
  )
