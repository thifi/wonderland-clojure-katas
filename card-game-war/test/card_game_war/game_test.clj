(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

(defn assert-round [p1 p2 winner]
  (let [res (play-round p1 p2)]
    (is
     (case winner
       :player1 (= res [[p1 p2] nil])
       :player2 (= res [nil [p1 p2]])))))

;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (assert-round [:heart 2] [:diamond 3] :player2))
  (testing "queens are higher rank than jacks"
    (assert-round [:club :king] [:spade :jack] :player1))
  (testing "kings are higher rank than queens"
    (assert-round [:club :queen] [:spade :king] :player2))
  (testing "aces are higher rank than kings"
    (assert-round [:spade :ace] [:club :king] :player1))
  (testing "if the ranks are equal, clubs beat spades"
    (assert-round [:spade :ace] [:club :ace] :player2))
  (testing "if the ranks are equal, diamonds beat clubs"
    (assert-round [:club :ace] [:diamond :ace] :player2))
  (testing "if the ranks are equal, hearts beat diamonds"
    (assert-round [:heart :ace] [:diamond :ace] :player1)))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is
     (let [result (apply play-game (deal-cards))]
       (case result
         :player1 true
         :player2 true
         false))))
  (testing "the player loses when they run out of cards"
    (is
     (let [half        (/ (count cards) 2)
           ranked-deck (sort-by rank-index cards)
           [p1 p2]     (split-at half ranked-deck)
           result      (play-game p1 p2)]
       (= result :player2))))
  (testing "the player loses when they run out of cards"
    (is
     (let [half        (/ (count cards) 2)
           ranked-deck (sort-by rank-index cards)
           [p1 p2]     (reverse (split-at half ranked-deck))
           result      (play-game p1 p2)]
       (= result :player1)))))

