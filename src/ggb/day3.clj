(ns ggb.day3
  (:require [clojure.string :as str]))

(defn to-num-list
  [l]
  (map #(Character/getNumericValue %) l))

(def input
  (->> (slurp "data/day3.txt")
       str/split-lines
       (map to-num-list)))

(defn joltage-helper
  [[l res] n]
  (let [slice (drop-last n l)
        lm (apply max slice)
        lk (str/index-of (apply str slice) (str lm))]
    [(drop (inc lk) l) (conj res lm)]))

(defn joltage
  [n l]
  (->> (reverse (range n))
       (reduce joltage-helper [l []])
       second
       (apply str)
       parse-long))

;; Part 1
(->> input
     (map (partial joltage 2))
     (reduce +))

;; Part 2
(->> input
     (map (partial joltage 12))
     (reduce +))