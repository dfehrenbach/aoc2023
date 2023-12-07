
(ns day6.core
  (:require [clojure.string :as string]))

(defn interpret-races [input]
  (let [[time-line distance-line] (string/split-lines input)
        times (mapv read-string (re-seq #"\d+" time-line))
        distances (mapv read-string (re-seq #"\d+" distance-line))]
    (mapv (fn [[time distance]]
            {:time time :distance distance})
          (mapv vector times distances))))

(def test-input
  (->> "src/day6/test.txt"
       slurp
       interpret-races))

(def input
  (->> "src/day6/input.txt"
       slurp
       interpret-races))

(defn round-up [x]
  (if (= x (Math/ceil x))
    (inc x)
    (Math/ceil x)))

(defn round-down [x]
  (if (= x (Math/floor x))
    (dec x)
    (Math/floor x)))

(defn solve [time distance]
  (let [b (- time)
        c distance
        d (Math/sqrt (- (* b b) (* 4 c)))
        x1 (/ (- (- b) d) 2)
        x2 (/ (+ (- b) d) 2)]
    [(round-up x1) (round-down x2)]))


(defn part1 [input]
  (->> input
       (map (fn [m]
              (let [[start end] (solve (:time m) (:distance m))]
                (inc (- end start)))))
       (reduce *)
       int))

(defn part2 [input]
  (->> input
       (reduce (fn [acc m]
                 (-> acc
                     (update :time (fn [t nt] (string/join [t nt])) (:time m))
                     (update :distance (fn [d nd] (string/join [d nd])) (:distance m)))))
       ((fn [{:keys [time distance]}]
          (let [[start end] (solve (read-string time) (read-string distance))]
            (inc (- end start)))))
       int))

(comment
  test-input
  ;; => [{:time 7, :distance 9} {:time 15, :distance 40} {:time 30, :distance 200}]

  (solve 30 200)
  ;; => [11.0 19.0]

  (part1 test-input)
  ;; => 288

  (part1 input)
  ;; => 316800

  (part2 test-input)
  ;; => 71503

  (part2 input)
  ;; => 45647654


  0)
