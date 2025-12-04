(ns ggb.day2
  (:require [clojure.string :as str]))

(def input 
  (->> (slurp "data/day4.txt")
       str/split-lines
       (map seq)
       (map vec)
       vec))

(defn get2d 
  [grid [x y]]
  (when (and (<= 0 x (dec (count grid)))
             (<= 0 y (dec (count (grid x)))))
    (get-in grid [x y])))

(def directions 
  [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn neighbors
  [grid [x y]]
  (map
   (fn [[dx dy]]
     (get2d grid [(+ x dx) (+ y dy)]))
   directions))

(defn find-neighbors 
  [grid]
  (for [x (range (count grid))
        y (range (count (grid x)))]
    (when (= \@ (get2d grid [x y])) 
      [[x y] (neighbors grid [x y])])))

(defn less-than-4
  [row] 
  (->> row
       second
       (map #(if (= \@ %) 1 0))
       (reduce +)
       (>= 3)))

(defn get-updates
  [grid]
  (->> grid 
       find-neighbors
       (filter some?)
       (filter less-than-4)))

;; Part 1
(->> input 
     get-updates
     count)

;; Part 2
(defn update-grid 
  [updates grid]
  (reduce (fn [acc val] 
            (assoc-in acc (first val) \.)) grid updates))

(defn get-total-rolls
  [acc grid]
  (let [updates (get-updates grid)
        n-changes (count updates)]
    (if (= 0 n-changes)
      acc
      (get-total-rolls (+ acc n-changes) (update-grid updates grid)))))

(get-total-rolls 0 input)

