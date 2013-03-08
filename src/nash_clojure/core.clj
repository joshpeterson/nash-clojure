(ns nash-clojure.core (:gen-class))
(use 'clojure.math.numeric-tower)
(use '[clojure.tools.cli :only [cli]])

;
; Two player game strategy generation
;

(defn vector-bit-or [& vectors]
  (apply map bit-or vectors))

(defn vector-bit-and [& vectors]
  (apply map bit-and vectors))

(defn one-index [row-selector number-of-columns strategy-index]
  (+ (* row-selector number-of-columns)
     (mod (quot strategy-index (expt number-of-columns row-selector))
          number-of-columns)))

(defn one-indexes [number-of-rows number-of-columns strategy-index]
  (for [row-selector (range number-of-rows)]
    (one-index row-selector number-of-columns strategy-index)))

(defn player-agnostic-strategy [number-of-rows number-of-columns strategy-index]
  (apply vector-bit-or (map #(assoc (vec (repeat (* number-of-rows number-of-columns) 0)) % 1)
                             (one-indexes number-of-rows number-of-columns strategy-index))))

(defn column-rank-order [number-of-columns player-agnostic-strategy]
  (flatten (vec (apply map vector (partition number-of-columns player-agnostic-strategy)))))

(defn player-one-strategy [number-of-rows number-of-columns strategy-index]
  (column-rank-order number-of-columns
                     (player-agnostic-strategy number-of-rows number-of-columns strategy-index)))

(defn player-two-strategy [number-of-rows number-of-columns strategy-index]
  (player-agnostic-strategy number-of-rows number-of-columns strategy-index))

;
; Nash game categorization
;

(defn pmap-mine
  "Like map, except f is applied in parallel. Semi-lazy in that the
  parallel computation stays ahead of the consumption, but doesn't
  realize the entire result unless required. Only useful for
  computationally intensive functions where the time of f dominates
  the coordination overhead."
  {:added "1.0"
   :static true}
  ([n f coll]
   (let [rets (map #(future (f %)) coll)
         step (fn step [[x & xs :as vs] fs]
                (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (deref x) (step xs (rest s)))
                   (map deref vs))))]
     (step rets (drop n rets)))))

(defn categorize-nash-solutions [player-one-strategy player-two-strategy]
  (count (take 2 (filter #{1} (vector-bit-and player-one-strategy player-two-strategy)))))

(defn categorize-nash-game [number-of-rows number-of-columns game-index]
  (categorize-nash-solutions (player-one-strategy number-of-rows number-of-columns
                                                  (quot game-index (* number-of-rows number-of-columns)))
                             (player-two-strategy number-of-rows number-of-columns
                                                  (mod game-index (* number-of-rows number-of-columns)))))

(defn number-of-nash-games [number-of-rows number-of-columns]
  (* (expt number-of-rows number-of-columns) (expt number-of-columns number-of-rows)))

(defn partition-nash-games [number-of-partitions number-of-rows number-of-columns games]
  (let [number-of-games (number-of-nash-games number-of-rows number-of-columns)
        partitions (partition-all (quot number-of-games number-of-partitions) games)]
    (if (= 0 (rem (count games) number-of-partitions))
      partitions
      (conj (take (- (+ number-of-partitions 1) 2) partitions) (apply concat (take-last 2 partitions))))))

(defn categorize-given-nash-games [number-of-rows number-of-columns game-indices]
  (frequencies (pmap #(categorize-nash-game number-of-rows number-of-columns %) game-indices)))

(defn categorize-nash-games [number-of-rows number-of-columns number-of-partitions]
  (let [number-of-games (number-of-nash-games number-of-rows number-of-columns)]
    (into (sorted-map) (reduce #(merge-with + %1 %2) 
                               (map #(categorize-given-nash-games number-of-rows number-of-columns %)
                                    (partition-nash-games number-of-partitions number-of-rows
                                                          number-of-columns (range number-of-games)))))))

(defn pcategorize-nash-games [number-of-rows number-of-columns number-of-partitions]
  (let [number-of-games (number-of-nash-games number-of-rows number-of-columns)]
    (into (sorted-map) (reduce #(merge-with + %1 %2)
                               (pmap-mine number-of-partitions
                                          #(categorize-given-nash-games number-of-rows number-of-columns %)
                                          (partition-nash-games number-of-partitions number-of-rows
                                                                number-of-columns
                                                                (range number-of-games)))))))
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
