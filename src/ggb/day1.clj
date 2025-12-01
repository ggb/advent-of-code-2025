(ns ggb.day1
  (:require [clojure.string :as str]))

(def max-ticks 100)

(defn calc-pos 
  [direction position ticks]
  (mod (+ ((if (= \L direction) - +) position ticks)
          max-ticks)
       max-ticks))

(defn count-zeros
  [direction position ticks]
  (->> (range 1 (inc ticks))
       (filter #(zero? (mod ((if (= \L direction) - +) position %) 100)))
       count))

(defn dialing
  [results [direction ticks]]
  (let [[position zeros] (first results)
        new-pos (calc-pos direction position ticks)
        add-zeros (count-zeros direction position ticks)]
    (cons [new-pos (+ zeros add-zeros)] results)))

(def prep (->> (slurp "data/day1.txt")
               str/split-lines
               (map #(vec [(first %) (parse-long (subs % 1))]))
               (reduce dialing '([50 0]))))

;; Part 1
(->> prep 
     (filter #(= 0 (first %)))
     count)

;; Part 2
(->> prep 
     first
     second)