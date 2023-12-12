(ns day8.core
  (:require [clojure.string :as string]))

(defn build-map [m item]
  (let [[k l r] (re-seq #"[A-Z\d]+" item)]
    (assoc m k [l r])))

(defn interpret-input [input]
  (let [[instructions maps] input]
    {:instructions (vec instructions) :network (reduce build-map {} (string/split-lines maps))}))

(defn get-input [path]
  (->> path
       slurp
       (#(string/split % #"\r?\n\r?\n"))
       interpret-input))

(def test1-input
  (get-input "src/day8/test1.txt"))

(def test2-input
  (get-input "src/day8/test2.txt"))

(def test3-input
  (get-input "src/day8/test3.txt"))

(def input
  (get-input "src/day8/input.txt"))

(defn setup-state [input current-node]
  {:instructions (:instructions input)
   :network (:network input)
   :current-node current-node
   :instruction-index 0})

(defn traverse-map [{:keys [instructions network current-node instruction-index] :as state}]
  (let [instruction (nth instructions (mod instruction-index (count instructions)))
        next-node (if (= instruction \L)
                    (first (get network current-node))
                    (second (get network current-node)))]
    (-> state
        (assoc :current-node next-node)
        (update :instruction-index inc))))

(defn part1 [input]
  (let [state (setup-state input "AAA")
        end-state (drop-while #(not= (:current-node %) "ZZZ") (iterate traverse-map state))]
    (->> end-state first :instruction-index)))


(defn gcd [a b]
  (if (= b 0) a
      (recur b (mod a b))))

(defn least-common-multiple [a b]
  (/ (* a b) (gcd a b)))

(defn part2 [input]
  (let [ghosts (filter #(string/ends-with? % "A") (keys (:network input)))
        states (pmap #(setup-state input %) ghosts)
        final-states (pmap (fn [state]
                             (first (drop-while #(not (string/ends-with? (:current-node %) "Z")) (iterate traverse-map state))))
                           states)]
    (reduce least-common-multiple (map :instruction-index final-states))))

(comment
  test2-input
  ;; => {:instructions [\L \L \R], :network {"AAA" ["BBB" "BBB"], "BBB" ["AAA" "ZZZ"], "ZZZ" ["ZZZ" "ZZZ"]}}

  (part1 test1-input)
  ;; => 2

  (part1 test2-input)
  ;; => 6

  (part1 input)
  ;; => 17621

  (part2 test3-input)
  ;; => 6

  (part2 input)
  ;; => 20685524831999

  0)
