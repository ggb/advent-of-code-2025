(ns ggb.day8
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [clojure.set :as set]))

(def input
  (->> (slurp "data/day8.txt")
       str/split-lines
       (map #(str/split % #","))
       (map #(map parse-long %))
       vec))

(def euclidian
  (memoize (fn
             [p q]
             (->> (map #(- %1 %2) p q)
                  (map #(math/pow % 2))
                  (reduce +)))))

(defn distances
  [l]
  (->> (for [[i ps] (map-indexed vector l)
             [j qs] (map-indexed vector l)
             :when (< i j)]
         [(euclidian ps qs) (list ps qs)])
       (filter #(> (first %) 0.0))
       (sort-by first)))

(defn add-pairs [clusters [a b]]
  (let [involved (filter #(or (% a) (% b)) clusters)
        uninvolved (remove #(or (% a) (% b)) clusters)]
    (cond
      (empty? involved) (conj clusters #{a b})
      (= 1 (count involved)) (conj uninvolved (conj (first involved) a b))
      :else (let [merged (apply set/union involved)]
              (conj uninvolved (conj merged a b))))))

(defn build-clusters [pairs]
  (reduce add-pairs [] pairs))

(def dists 
  (->> input
       (distances)
       (map second)))

;; Part 1
(->> dists
     (take 1000)
     (build-clusters)
     (map count)
     (sort)
     (take-last 3)
     (apply *))

;; Part 2
(defn keep-connecting
  [clusters pairs [p q]]
  (if (some #(= (count %) (count input)) clusters)
    (* (first p) (first q))
    (recur (add-pairs clusters (first pairs)) (rest pairs) (first pairs))))

(keep-connecting [] dists [0 0])