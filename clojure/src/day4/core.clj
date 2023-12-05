
(ns day4.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn read-card [line]
  (let [[_ card-id winning-numbers numbers] (re-find #"Card\s+(\d+):(.*)\|(.*)" line)
        winning-numbers (map read-string (re-seq #"\d+" winning-numbers))
        numbers (map read-string (re-seq #"\d+" numbers))]
    {:card card-id
     :winning-numbers winning-numbers
     :numbers numbers}))

(def test-input
  (->> "src/day4/test.txt"
       slurp
       string/split-lines
       (map read-card)))

(def input
  (->> "src/day4/input.txt"
       slurp
       string/split-lines
       (map read-card)))

(defn card-matches [card]
  (set/intersection (set (:numbers card)) (set (:winning-numbers card))))

(defn score-card [matches]
  (if (empty? matches) 0
      (Math/pow 2 (dec (count matches)))))

(defn part1 []
  (->> input
       (map card-matches)
       (map score-card)
       (reduce +)
       int))

(defn setup-cards [matches]
  {:card-count 1 :matches matches})

(defn increment-card-count [cards cardIndex card]
  (if (get cards cardIndex)
    (update-in cards [cardIndex 1] #(update % :card-count + (:card-count card)))
    cards))

(defn increment-cards [cards [index _]]
  (let [[_ card] (get cards index)]
    (reduce (fn [cards cardIndex]
              (increment-card-count cards cardIndex card))
            cards
            (range (inc index) (+ (inc index) (count (:matches card)))))))

(defn part2 []
  (->> input
       (map card-matches)
       (mapv setup-cards)
       (map-indexed vector)
       (vec)
       (#(reduce increment-cards % %))
       (map (comp :card-count second))
       (reduce +)))

(comment
  (part1)
  ;; => 21959

  (part2)
  ;; => 5132675

  0)
