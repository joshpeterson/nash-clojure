(ns
  #^{:author "Josh Peterson"
     :doc "Functions used to categorize Nash solutions to two-player games"} 
  nash-clojure.core (:gen-class))
(use 'clojure.math.numeric-tower)
(use '[clojure.tools.cli :only [cli]])

;
; Two player game strategy generation
;

(defn vector-bit-or
  "Perform a bitwise OR operation on two or more vectors."
  [& vectors]
  (apply map bit-or vectors))

(defn vector-bit-and
  "Perform a bitwise AND operation on two or more vectors."
  [& vectors]
  (apply map bit-and vectors))

(defn one-index
  "Given a player-agnostic strategy index, find the index of the 1 for the given row." 
  [row-selector number-of-columns strategy-index]
  (+ (* row-selector number-of-columns)
     (mod (quot strategy-index (expt number-of-columns row-selector))
          number-of-columns)))

(defn one-indexes
  "Given a player-agnostic strategy index, find the index of the 1 for each row."
  [number-of-rows number-of-columns strategy-index]
  (for [row-selector (range number-of-rows)]
    (one-index row-selector number-of-columns strategy-index)))

(defn player-agnostic-strategy
  "Given a strategy index, find the player-agnostics strategy for a game of the given size."
  [number-of-rows number-of-columns strategy-index]
  (apply vector-bit-or (map #(assoc (vec (repeat (* number-of-rows number-of-columns) 0)) % 1)
                             (one-indexes number-of-rows number-of-columns strategy-index))))

(defn column-rank-order
  "Apply the column rank order transformation (effectivel a transpose) to the given player
   agnostic strategy."
  [number-of-columns player-agnostic-strategy]
  (flatten (vec (apply map vector (partition number-of-columns player-agnostic-strategy)))))

(defn player-one-strategy
  "Find the player 1 strategy for the given strategy index."
  [number-of-rows number-of-columns strategy-index]
  (column-rank-order number-of-columns
                     (player-agnostic-strategy number-of-rows number-of-columns strategy-index)))

(defn player-two-strategy
  "Find the player 2 strategy for the given strategy index."
  [number-of-rows number-of-columns strategy-index]
  (player-agnostic-strategy number-of-rows number-of-columns strategy-index))

;
; Nash game categorization
;

(defn categorize-nash-solution
  "Categorize the Nash solutions for the given two player game."
  [player-one-strategy player-two-strategy]
  (count (take 2 (filter #{1} (vector-bit-and player-one-strategy player-two-strategy)))))

(defn categorize-nash-game
  "Generate the player 1 and 2 strategies and categorize the Nash solutions for the given game index."
  [number-of-rows number-of-columns game-index]
  (categorize-nash-solution (player-one-strategy number-of-rows number-of-columns
                                                 (quot game-index (expt number-of-rows number-of-columns)))
                            (player-two-strategy number-of-rows number-of-columns
                                                 (mod game-index (expt number-of-rows number-of-columns)))))

(defn number-of-nash-games
  "Determine the number of Nash games of a given size"
  [number-of-rows number-of-columns]
  (* (expt number-of-rows number-of-columns) (expt number-of-columns number-of-rows)))

(defn partition-nash-games
  "Determine the start and end indices of each group of Nash games"
  [number-of-partitions number-of-games]
  (let [entries-per-partition (quot number-of-games number-of-partitions)
        partitions (map #(conj [%] (+ % (- entries-per-partition 1)))
                        (filter #(and (>= (- number-of-games %) entries-per-partition)
                                      (= 0 (mod %1 entries-per-partition)))
                                (range number-of-games)))]
    (if (= 0 (rem number-of-games number-of-partitions))
      partitions
      (let [first-indices (take (- number-of-partitions 1) partitions)
            last-indices-start (nth (nth (take-last 1 partitions) 0) 0)
            new-last-indices (conj [] last-indices-start (- number-of-games 1))]
       (apply concat (conj () (conj () new-last-indices) first-indices))))))

(defn categorize-given-nash-games
  "Categorize the Nash solutions for games with the given games indoces."
  [number-of-rows number-of-columns start-and-end-indices]
  (frequencies (map #(categorize-nash-game number-of-rows number-of-columns %)
                    (range (nth start-and-end-indices 0) (+ (nth start-and-end-indices 1) 1)))))

(defn categorize-nash-games
  "Categorize the Nash solutions for games of a given size, using the given number of paritions
   using one thread."
  [number-of-rows number-of-columns number-of-partitions]
  (let [number-of-games (number-of-nash-games number-of-rows number-of-columns)]
    (into (sorted-map) (reduce #(merge-with + %1 %2) 
                               (map #(categorize-given-nash-games number-of-rows number-of-columns %)
                                    (partition-nash-games number-of-partitions number-of-games))))))

(defn pcategorize-nash-games
  "Categorize the Nash solutions for games of a given size, using the given number of partitions
   using one thread per partition."
  [number-of-rows number-of-columns number-of-partitions]
  (let [number-of-games (number-of-nash-games number-of-rows number-of-columns)]
    (into (sorted-map) (reduce #(merge-with + %1 %2)
                               (pmap #(categorize-given-nash-games number-of-rows number-of-columns %)
                                     (partition-nash-games number-of-partitions number-of-games))))))
;
; Main function
;

(defn -main [& args]
  (let [[options args banner]
    (cli args
      ["-h" "--help" "Show help" :flag true]
      ["-r" "--rows" "Number of rows in the game" :parse-fn #(Integer. %)] 
      ["-c" "--columns" "Number of columns in the game" :parse-fn #(Integer. %)]
      ["-t" "--threads" "Number of threads to use" :parse-fn #(Integer. %)])]
  (when (:help options)
      (println banner)
      (System/exit 0))
    
    (println (time (pcategorize-nash-games (:rows options) (:columns options) (:threads options)))))
  (System/exit 0))
