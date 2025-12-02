(ns ggb.day2
  (:require [clojure.string :as str]))

(def input 
  (->> (str/split (slurp "data/day2.txt") #",")
       (map #(str/split % #"-"))
       (map (fn [[fst snd]]
              (range (parse-long fst) (inc (parse-long snd)))))
       (reduce concat)
       (map str)))

;; Part 1
(->> input
     (filter #(even? (count %)))
     (filter #(apply = (partition (/ (count %) 2) %)))
     (map parse-long)
     (reduce +))

;; Part 2
(defn valid?
  [s]
  (let [len (count s)]
    (->> (range 1 len)
         (filter #(= 0 (mod len %)))
         (map #(partition % s))
         (map #(apply = %))
         (some true?))))

(->> input
     (map str)
     (filter valid?)
     (map parse-long)
     (reduce +))