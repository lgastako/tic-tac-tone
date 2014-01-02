(ns tic-tac-tone.core-test
  (:require [clojure.test :refer :all]
            [tic-tac-tone.core :refer :all]))

(deftest test-which-spot
  (testing "Mapping row/col to tic-tac-toe spot"
    (is (= 2 (which-spot 0 6)))))

(deftest test-empty-board
  (testing "Empty board layout"
    (is (= empty-board
           [[:e :e :y :e :e :y :e :e]
            [:e :e :y :e :e :y :e :e]
            [:y :y :y :y :y :y :y :y]
            [:e :e :y :e :e :y :e :e]
            [:e :e :y :e :e :y :e :e]
            [:y :y :y :y :y :y :y :y]
            [:e :e :y :e :e :y :e :e]
            [:e :e :y :e :e :y :e :e]]))))

(deftest test-valid-move
  (testing "Valid moves"
    (is (valid-move? (atom {:spots [:e :e :e
                                    :e :e :e
                                    :e :e :e]}) 1))))

(deftest test-spots-full?
  (testing "Check if spots are full"
    (is (spots-full? (atom {:spots [:g :g :r
                                    :r :g :g
                                    :g :r :r]})))
    (is (not (spots-full? (atom {:spots [:g :g :r
                                         :r :g :g
                                         :g :r :e]}))))))

(def test-find-win
  (testing "Finding a winner"
    (is (= ))))
