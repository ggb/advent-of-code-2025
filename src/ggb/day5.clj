(ns ggb.day5
  (:require [clojure.string :as str]))

(def input
  (-> (slurp "data/day5.txt")
      (str/split #"\n\n")))

(def database
  (->> input
       first
       str/split-lines
       (map #(str/split % #"-"))
       (map (fn [[fst snd]]
              [(parse-long fst) (parse-long snd)]))))

(def ids
  (->> input
       second
       str/split-lines
       (map parse-long)))

;; Part 1
(defn check-in?
  [id]
  (some (fn [[fst snd]] (<= fst id snd)) database))

(->> ids
     (filter check-in?)
     count)

;; Part 2
(defn collapse-ranges
  [done todo]
  (if (< (count todo) 2)
    (concat done todo)
    (let [[current-fst current-snd] (first todo)
          [next-fst next-snd] (second todo)]
      (cond  
        (<= next-fst current-snd next-snd) (collapse-ranges done (cons [current-fst next-snd] (drop 2 todo)))
        (<= current-fst next-fst next-snd current-snd) (collapse-ranges done (cons [current-fst current-snd] (drop 2 todo)))
        :else (collapse-ranges (cons (first todo) done) (rest todo))))))

(->> database
     (sort-by first)
     (collapse-ranges '())
     (map #(- (second %) (first %)))
     (map inc)
     (reduce +))