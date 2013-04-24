(ns nash-clojure.test.core
  (:use [nash-clojure.core])
  (:use [clojure.test]))

(deftest categorize-all-two-by-two-games
  (is (= {0 2, 1 12, 2 2} (categorize-nash-games 2 2 1))))

(deftest categorize-all-three-by-three-games
  (is (= {0 156, 1 423, 2 150} (categorize-nash-games 3 3 1))))

(deftest categorize-all-two-by-two-games-with-two-partitions
  (is (= {0 2, 1 12, 2 2} (categorize-nash-games 2 2 2))))

(deftest categorize-all-three-by-three-games-with-two-partitions
  (is (= {0 156, 1 423, 2 150} (categorize-nash-games 3 3 2))))

(deftest partition-three-by-three-games-with-two-partitions
  (is (= (seq '(365 364)) (map count (partition-nash-games 2 3 3 (range 729))))))

(deftest partition-three-by-three-games-with-three-partitions
  (is (= (seq '(243 243 243)) (map count (partition-nash-games 3 3 3 (range 729))))))

(deftest partition-two-by-two-games-with-two-partitions
  (is (= (seq '(8 8)) (map count (partition-nash-games 2 2 2 (range 16))))))

(deftest partition-two-by-two-games-with-three-partitions
  (is (= (seq '(6 5 5)) (map count (partition-nash-games 3 2 2 (range 16))))))

(deftest finds-unique-nash-solutions
  (is (= 1 (categorize-nash-solution [1 0 1 0] [1 1 0 0]))))

(deftest finds-non-unique-nash-solutions
  (is (= 2 (categorize-nash-solution [1 0 0 1] [1 0 0 1]))))

(deftest finds-no-nash-solutions
  (is (= 0 (categorize-nash-solution [1 0 0 1] [0 1 1 0]))))

(deftest categorize-two-by-two-game-0
  (is (= 1 (categorize-nash-game 2 2 0))))

(deftest categorize-two-by-two-game-1
  (is (= 1 (categorize-nash-game 2 2 1))))

(deftest categorize-two-by-two-game-2
  (is (= 1 (categorize-nash-game 2 2 2))))

(deftest categorize-two-by-two-game-3
  (is (= 1 (categorize-nash-game 2 2 3))))

(deftest categorize-two-by-two-game-4
  (is (= 1 (categorize-nash-game 2 2 4))))

(deftest categorize-two-by-two-game-5
  (is (= 2 (categorize-nash-game 2 2 5))))

(deftest categorize-two-by-two-game-6
  (is (= 0 (categorize-nash-game 2 2 6))))

(deftest categorize-two-by-two-game-7
  (is (= 1 (categorize-nash-game 2 2 7))))

(deftest categorize-two-by-two-game-8
  (is (= 1 (categorize-nash-game 2 2 8))))

(deftest categorize-two-by-two-game-9
  (is (= 0 (categorize-nash-game 2 2 9))))

(deftest categorize-two-by-two-game-10
  (is (= 2 (categorize-nash-game 2 2 10))))

(deftest categorize-two-by-two-game-11
  (is (= 1 (categorize-nash-game 2 2 11))))

(deftest categorize-two-by-two-game-12
  (is (= 1 (categorize-nash-game 2 2 12))))

(deftest categorize-two-by-two-game-13
  (is (= 1 (categorize-nash-game 2 2 13))))

(deftest categorize-two-by-two-game-14
  (is (= 1 (categorize-nash-game 2 2 14))))

(deftest categorize-two-by-two-game-15
  (is (= 1 (categorize-nash-game 2 2 15))))

(deftest finds-first-two-by-two-player-one-strategy
  (is (= [1 1 0 0] (player-one-strategy 2 2 0))))

(deftest finds-second-two-by-two-player-one-strategy
  (is (= [0 1 1 0] (player-one-strategy 2 2 1))))

(deftest finds-third-two-by-two-player-one-strategy
  (is (= [1 0 0 1] (player-one-strategy 2 2 2))))

(deftest finds-fourth-two-by-two-player-one-strategy
  (is (= [0 0 1 1] (player-one-strategy 2 2 3))))

(deftest finds-first-two-by-two-player-two-strategy
  (is (= [1 0 1 0] (player-two-strategy 2 2 0))))

(deftest finds-second-two-by-two-player-two-strategy
  (is (= [0 1 1 0] (player-two-strategy 2 2 1))))

(deftest finds-third-two-by-two-player-two-strategy
  (is (= [1 0 0 1] (player-two-strategy 2 2 2))))

(deftest finds-fourth-two-by-two-player-two-strategy
  (is (= [0 1 0 1] (player-two-strategy 2 2 3))))

(deftest finds-first-two-by-two-player-agnostic-strategy
  (is (= [1 0 1 0] (player-agnostic-strategy 2 2 0))))

(deftest finds-second-two-by-two-player-agnostic-strategy
  (is (= [0 1 1 0] (player-agnostic-strategy 2 2 1))))

(deftest finds-third-two-by-two-player-agnostic-strategy
  (is (= [1 0 0 1] (player-agnostic-strategy 2 2 2))))

(deftest finds-fourth-two-by-two-player-agnostic-strategy
  (is (= [0 1 0 1] (player-agnostic-strategy 2 2 3))))

(deftest finds-seventeenth-four-by-four-player-agnostic-strategy
  (is (= [1 0 0 0 1 0 0 0 0 1 0 0 1 0 0 0] (player-agnostic-strategy 4 4 16))))

(deftest finds-column-rank-order-for-first-two-by-two-player-agnostic-strategy
  (is (= [1 1 0 0] (column-rank-order 2 [1 0 1 0]))))

(deftest finds-column-rank-order-for-second-two-by-two-player-agnostic-strategy
  (is (= [0 1 1 0] (column-rank-order 2 [0 1 1 0]))))

(deftest finds-column-rank-order-for-third-two-by-two-player-agnostic-strategy
  (is (= [1 0 0 1] (column-rank-order 2 [1 0 0 1]))))

(deftest finds-column-rank-order-for-fourth-two-by-two-player-agnostic-strategy
  (is (= [0 0 1 1] (column-rank-order 2 [0 1 0 1]))))

(deftest verify-one-index-for-two-by-two-strategy-zero-row-zero
    (is (= 0 (one-index 0 2 0))))

(deftest verify-one-index-for-two-by-two-strategy-zero-row-one
    (is (= 2 (one-index 1 2 0))))

(deftest verify-one-index-for-two-by-two-strategy-one-row-zero
    (is (= 1 (one-index 0 2 1))))

(deftest verify-one-index-for-two-by-two-strategy-one-row-one
    (is (= 2 (one-index 1 2 1))))

(deftest verify-one-index-for-two-by-two-strategy-two-row-zero
    (is (= 0 (one-index 0 2 2))))

(deftest verify-one-index-for-two-by-two-strategy-two-row-one
    (is (= 3 (one-index 1 2 2))))

(deftest verify-one-index-for-two-by-two-strategy-three-row-zero
    (is (= 1 (one-index 0 2 3))))

(deftest verify-one-index-for-two-by-two-strategy-three-row-one
    (is (= 3 (one-index 1 2 3))))

(deftest verify-one-indexes-for-two-by-two-strategy-zero
         (is (= [0 2] (one-indexes 2 2 0))))

(deftest verify-one-indexes-for-two-by-two-strategy-one
         (is (= [1 2] (one-indexes 2 2 1))))

(deftest verify-one-indexes-for-two-by-two-strategy-two
         (is (= [0 3] (one-indexes 2 2 2))))

(deftest verify-one-indexes-for-two-by-two-strategy-three
         (is (= [1 3] (one-indexes 2 2 3))))
