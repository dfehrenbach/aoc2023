(ns day1.core
  (:require [clojure.string :as string]))

(def input
  (->> "src/day1/input.txt"
       slurp
       string/split-lines))

(defn get-number [string]
  (let [first (re-find  #"\d" string)
        last (re-find #"\d" (string/reverse string))]
    (Integer/parseInt (str first last))))

(defn part1 []
;; take each line, find the first number and last number, concatenate them, interpret as a number, then sum all
  (->> input
       (map get-number)
       (reduce +)))

(def word->num
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(def revword->num
  (zipmap (map string/reverse (keys word->num)) (vals word->num)))


(defn get-number2 [string]
  (let [first (re-find  #"one|two|three|four|five|six|seven|eight|nine|\d" string)
        firstNum (or (word->num first) first)
        last (re-find #"eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|\d" (string/reverse string))
        lastNum (or (revword->num last) last)]
    (Integer/parseInt (str firstNum lastNum))))

(defn part2 []
  (->> input
       (map get-number2)
       (reduce +)))

(comment
  (take 1 input)
  ;; => ("9vxfg")

  (part1)
  ;; => 54390

  (part2)
  ;; => 54277
  )
