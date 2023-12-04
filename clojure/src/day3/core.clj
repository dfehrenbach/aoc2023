
(ns day3.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def max-x 140)
(def max-y 140)

(defn re-fun
  [re s fun]
  (let [matcher (re-matcher re s)]
    (take-while some? (repeatedly #(if (.find matcher) (fun matcher) nil)))))

(defn re-seq-map [re s]
  (into {} (re-fun re s #(vector (.start %) (.group %)))))

(def input
  (->> "src/day3/input.txt"
       slurp
       string/split-lines))

(def input-grid
  (mapv vec input))

(def input-nums
  (map #(re-seq-map #"\d+" %) input))

(defn is-symbol? [c]
  (not (re-find #"[\d.]" (str c))))

(defn coords-around-str [[startX startY] s]
  ;; aaa at x=2, y=2 => [[1 1] [2 1] [3 1] [4 1] [5 1] [1 2] [3 2] [4 2] [5 2] [1 3] [2 3] [3 3] [4 3] [5 3]]
  (let [width (count s)
        xs (range (- startX 1) (+ startX width 1))
        ys (range (- startY 1) (+ startY 2))]
    (for [x xs
          y ys
          :when (and (>= x 0) (< x max-x)
                     (>= y 0) (< y max-y))]
      [x y])))

(defn is-part? [[x y] s]
  (some (fn [[x y]] (is-symbol? (get-in input-grid [y x])))
        (coords-around-str [x y] s)))

(defn filter-parts [y parts]
  (filter (fn [[x s]] (is-part? [x y] s))
          parts))

(defn part1 []
  (let [parts (map-indexed filter-parts input-nums)]
    (->> parts
         (mapcat #(map (comp read-string second) %))
         (reduce +))))

(def input-cogs
  (map #(re-seq-map #"\*" %) input))

(def cog-coords-list
  (->> input-cogs
       (map-indexed (fn [y cogs]
                      (mapv (fn [[x _]] [x y]) cogs)))
       (reduce concat)))

(defn transform-parts [y parts]
  (map (fn [[x s]]
         (let [coords (mapv #(vector % y)
                            (range x (+ x (count s))))]
           {:num (read-string s) :coords coords}))
       parts))

(def all-numbers
  (->> input-nums
       (map-indexed transform-parts)
       flatten))

(defn cog-next-to-num? [[x y] {coords :coords}]
  ;; true if intersection between coords and coords-around-str of x,y is not empty
  (seq (set/intersection (set coords) (set (coords-around-str [x y] "*")))))

(defn nums-next-to-cog [[x y]]
  (filter #(cog-next-to-num? [x y] %) all-numbers))

(defn part2 []
  (reduce + (for [cog cog-coords-list
                  :let [nums (nums-next-to-cog cog)]
                  :when (= 2 (count nums))]
              (reduce * (map :num nums)))))

(comment
  (part1)
  ;; => 527364

  (first input-nums)
  ;; => {19 "15", 25 "904", 39 "850", 59 "329", 81 "13", 119 "871", 126 "816", 133 "697"}

  (second input-cogs)
  ;; => {62 "*", 86 "*", 120 "*", 132 "*"}

  (first all-numbers)
  ;; => {:num 15, :coords [[19 0] [20 0]]}

  (part2)
  ;; => 79026871
  0)
