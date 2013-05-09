(ns
  #^{:author "Josh Peterson"
     :doc "Functions used to categorize Nash solutions to two-player games"} 
  core (:gen-class)

;
; This is the source code for clojure.tools.cli. It is included here so that
; the calls to Java interop can be changed to call to the corresponding
; C# methods.
;
(:use [clojure.string :only (replace)]
      [clojure.pprint :only (pprint cl-format)])
(:refer-clojure :exclude [replace]))

(defn- build-doc [{:keys [switches docs default]}]
  [(apply str (interpose ", " switches))
   (or (str default) "")
   (or docs "")])

(defn- banner-for [specs]
  (println "Usage:")
  (println)
  (let [docs (into (map build-doc specs)
                   [["--------" "-------" "----"]
                    ["Switches" "Default" "Desc"]])
        max-cols (->> (for [d docs] (map count d))
                      (apply map (fn [& c] (apply vector c)))
                      (map #(apply max %)))
        vs (for [d docs]
             (mapcat (fn [& x] (apply vector x)) max-cols d))]
    (doseq [v vs]
      (cl-format true "~{ ~vA  ~vA  ~vA ~}" v)
      (prn))))

(defn- name-for [k]
  (replace k #"^--no-|^--\[no-\]|^--|^-" ""))

(defn- flag-for [^String v]
  (not (.StartsWith v "--no-")))

(defn- opt? [^String x]
  (.StartsWith x "-"))

(defn- flag? [^String x]
  (.StartsWith x "--[no-]"))

(defn- end-of-args? [x]
  (= "--" x))

(defn- spec-for
  [arg specs]
  (->> specs
       (filter (fn [s]
                   (let [switches (set (s :switches))]
                     (contains? switches arg))))
       first))

(defn- default-values-for
  [specs]
  (reduce (fn [m s]
            (if (contains? s :default)
              (assoc m (:name s) (:default s))
              m))
          {} specs))

(defn- apply-specs
  [specs args]
  (loop [options    (default-values-for specs)
         extra-args []
         args       args]
    (if-not (seq args)
      [options extra-args]
      (let [opt  (first args)
            spec (spec-for opt specs)]
        (cond
         (end-of-args? opt)
         (recur options (into extra-args (vec (rest args))) nil)

         (and (opt? opt) (nil? spec))
         (throw (Exception. (str "'" opt "' is not a valid argument")))
         
         (and (opt? opt) (spec :flag))
         (recur (assoc options (spec :name) (flag-for opt))
                extra-args
                (rest args))

         (opt? opt)
         (recur (assoc options (spec :name) ((spec :parse-fn) (second args)))
                extra-args
                (drop 2 args))

         :default
         (recur options (conj extra-args (first args)) (rest args)))))))

(defn- switches-for
  [switches flag]
  (-> (for [^String s switches]
        (cond
         (and flag (flag? s))            [(replace s #"\[no-\]" "no-") (replace s #"\[no-\]" "")]
         (and flag (.StartsWith s "--")) [(replace s #"--" "--no-") s]
         :default                        [s]))
      flatten))

(defn- generate-spec
  [raw-spec]
  (let [[switches raw-spec] (split-with #(and (string? %) (opt? %)) raw-spec)
        [docs raw-spec]     (split-with string? raw-spec)
        options             (apply hash-map raw-spec)
        aliases             (map name-for switches)
        flag                (or (flag? (last switches)) (options :flag))]
    (merge {:switches (switches-for switches flag)
            :docs     (first docs)
            :aliases  (set aliases)
            :name     (keyword (last aliases))
            :parse-fn identity
            :flag     flag}
           (when flag {:default false})
           options)))

(defn cli
  "Parse the provided args using the given specs. Specs are vectors
  describing a command line argument. For example:

  [\"-p\" \"--port\" \"Port to listen on\" :default 3000 :parse-fn #(Integer/parseInt %)]

  First provide the switches (from least to most specific), then a doc
  string, and pairs of options.

  Valid options are :default, :parse-fn, and :flag. See
  https://github.com/clojure/tools.cli/blob/master/README.md for more
  detailed examples.

  Returns a vector containing a map of the parsed arguments, a vector
  of extra arguments that did not match known switches, and a
  documentation banner to provide usage instructions."
  [args & specs]
  (let [specs (map generate-spec specs)]
    (let [[options extra-args] (apply-specs specs args)
          banner  (with-out-str (banner-for specs))]
      [options extra-args banner])))

;
; This is the source code for clojure.math.numeric-tower that is used by the normal
; nash-clojure code. It is included here because I was unable to get the Clojure CLR
; comiler to load the numeric_tower.clj file correctly.
;

(def ^{:private true} minus (first [-' -]))
(def ^{:private true} mult (first [*' *]))
(def ^{:private true} plus (first [+' +]))
(def ^{:private true} dec* (first [dec' dec]))
(def ^{:private true} inc* (first [inc' inc]))

(defn- expt-int [base pow]
  (loop [n pow, y (num 1), z base]
    (let [t (even? n), n (quot n 2)]
      (cond
       t (recur n y (mult z z))
       (zero? n) (mult z y)
       :else (recur n (mult z y) (mult z z))))))

(defn expt
  "(expt base pow) is base to the pow power.
Returns an exact number if the base is an exact number and the power is an integer, otherwise returns a double."
  [base pow]
  (if (and (not (float? base)) (integer? pow))
    (cond
     (pos? pow) (expt-int base pow)
     (zero? pow) 1
     :else (/ 1 (expt-int base (minus pow))))
    (Math/pow base pow)))


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
      ["-r" "--rows" "Number of rows in the game" :parse-fn #(Int32/Parse %)] 
      ["-c" "--columns" "Number of columns in the game" :parse-fn #(Int32/Parse %)]
      ["-t" "--threads" "Number of threads to use" :parse-fn #(Int32/Parse %)])]
  (when (:help options)
      (println banner)
      (Environment/Exit 0))
    
    (println (time (pcategorize-nash-games (:rows options) (:columns options) (:threads options))))))
