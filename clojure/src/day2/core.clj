
(ns day2.core
  (:require [clojure.string :as string]))

(defn interpret-round [round]
  ;; 1 green, 2 red, 6 blue => {:green 1 :red 2 :blue 6}
  (let [[_ green] (re-find #"(\d+) green" round)
        [_ blue] (re-find #"(\d+) blue" round)
        [_ red] (re-find #"(\d+) red" round)]
    {:green (read-string (or green "0"))
     :blue (read-string (or blue "0"))
     :red (read-string (or red "0"))}))

(defn interpret-line [line]
  (let [[_ gameID] (re-find #"Game (\d+):" line)
        rounds (string/split (second (string/split line #": ")) #"; ")]
    {:game (read-string gameID)
     :rounds (map interpret-round rounds)}))

(def input
  (->> "src/day2/input.txt"
       slurp
       string/split-lines
       (map interpret-line)))

(def part1-limit {:green 13 :blue 14 :red 12})

(defn compare-round-to-limit [round limit]
  (and (<= (round :green) (limit :green))
       (<= (round :blue) (limit :blue))
       (<= (round :red) (limit :red))))

(defn compare-game-to-limit [game limit]
  (every? (fn [round] (compare-round-to-limit round limit)) (:rounds game)))

(defn part1 []
  (let [possible-games (filter (fn [game] (compare-game-to-limit game part1-limit)) input)]
    (println (count possible-games))
    (reduce + (map :game possible-games))))

(defn highest-cube-of-each-color [game]
  (let [rounds (:rounds game)
        green (apply max (map :green rounds))
        blue (apply max (map :blue rounds))
        red (apply max (map :red rounds))]
    {:green green :blue blue :red red}))

(defn cube-power [min-cubes]
  (let [green (min-cubes :green)
        blue (min-cubes :blue)
        red (min-cubes :red)]
    (* green blue red)))

(defn part2 []
  (->> input
       (map highest-cube-of-each-color)
       (map cube-power)
       (reduce +)))

(comment
  (part1)
  ;; => 2268

  (part2)
  ;; => 63542
  )
