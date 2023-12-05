
(ns day5.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn interpret-seeds [line]
  (let [seeds (re-seq #"\d+" line)]
    (map read-string seeds)))

(defn interpret-range [line]
  (->> line
       (re-seq #"\d+")
       (mapv read-string)
       ((fn [[d s l]]
          {:source-range [s (+ s l)] :destination-range [d (+ d l)] :shift (- d s)}))))

(defn interpret-map [line]
  (let [[line & maps] (string/split-lines line)
        [_ source-key destination-key] (re-find #"([a-z]+)-to-([a-z]+)" line)
        ranges (map interpret-range maps)]
    {:source (keyword source-key)
     :destination (keyword destination-key)
     :ranges ranges}))

(defn interpret-input [input]
  (let [[seeds maps] (split-at 1 input)]
    {:seeds (interpret-seeds (first seeds))
     :maps (map interpret-map maps)}))

(def test-input
  (->> "src/day5/test.txt"
       slurp
       (#(string/split % #"\r?\n\r?\n"))
       interpret-input))

(def input
  (->> "src/day5/input.txt"
       slurp
       (#(string/split % #"\r?\n\r?\n"))
       interpret-input))

(defn convert-over-ranges [source ranges]
  (let [ranges (filter (fn [m]
                         (let [[start end] (:source-range m)]
                           (<= start source (dec end))))
                       ranges)]
    (if (seq ranges)
      (+ source (:shift (first ranges)))
      source)))

(comment
  (convert-over-ranges 79 (-> test-input :maps first :ranges))
  ;; => 81

  (convert-over-ranges 14 (-> test-input :maps first :ranges))
  ;; => 14

  (convert-over-ranges 55 (-> test-input :maps first :ranges))
  ;; => 57

  (convert-over-ranges 13 (-> test-input :maps first :ranges))
  ;; => 13
  0)

(defn convert-all-sources [sources m]
  (map (fn [source]
         (convert-over-ranges source (:ranges m)))
       sources))

(defn convert [{:keys [seeds maps]}]
  (reduce convert-all-sources seeds maps))

(defn part1 [input]
  (->> input
       convert
       (apply min)))

(defn brute-build-seed-list [seeds]
  (let [list-descriptions (partition 2 2 seeds)]
    (mapcat (fn [[start length]] (range start (+ start length))) list-descriptions)))

(defn part2 [input]
  (-> input
      (update :seeds brute-build-seed-list)
      convert
      (#(apply min %))))

(comment

  (:seeds test-input)
  ;; => (79 14 55 13)

  (first (:maps test-input))
  ;; => {:source :seed,
  ;;     :destination :soil,
  ;;     :ranges
  ;;     ({:source-range [98 100], :destination-range [50 52], :shift -48}
  ;;      {:source-range [50 98], :destination-range [52 100], :shift 2})}

  (convert test-input)
  ;; => (82 43 86 35)

  (part1 test-input)
  ;; => 35

  (convert input)
  ;; => (940608699
  ;;     977187125
  ;;     2852007167
  ;;     3231183623
  ;;     2510726952
  ;;     368168133
  ;;     1590835046
  ;;     336937652
  ;;     3066315671
  ;;     895170702
  ;;     2627301235
  ;;     331445006
  ;;     3829090675
  ;;     3249114790
  ;;     3483972752
  ;;     1845776732
  ;;     359042615
  ;;     4090870626
  ;;     4238424822
  ;;     1056365404)

  (part1 input)
  ;; => 331445006


  (part2 test-input)
  ;; => 46

  (part2 input)


  0)
