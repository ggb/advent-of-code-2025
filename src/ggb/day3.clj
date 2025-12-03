(ns ggb.day3
  (:require [clojure.string :as str]))


(defn to-num-list
  [l]
  (map #(Character/getNumericValue %) l))

(defn max-or-nil
  [val]
  (if (empty? val) nil (apply max val)))

(defn find-max
  [l]
  (let [lm (apply max l)
        lk (str/index-of (apply str l) (str lm))
        [left right] (split-at lk l)
        left-max (max-or-nil left)
        right-max (max-or-nil (rest right))]
    (max (parse-long (str left-max lm))
         (parse-long (str lm right-max)))))

;; Part 1
(->> (slurp "data/day3.txt")
     str/split-lines
     (map to-num-list)
     (map find-max)
     (reduce +))

;; Part 2
;; noch keine Zeit gehabt... 