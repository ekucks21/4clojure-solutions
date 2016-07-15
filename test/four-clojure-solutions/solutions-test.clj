(ns four-clojure-solutions.solutions-test
  (:use  [clojure.test])
  (:use  [four-clojure-solutions.solutions]))

(deftest subsets-test
  (is (= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})))

(deftest keys-and-values-test
  (is (= (keys-and-values []) {}))
  (is (= (keys-and-values [:a 1]) {:a [1]}))
  (is (= (keys-and-values [:a 1, :b 2]) {:a [1], :b [2]}))
  (is (= (keys-and-values [:a 1 2 3, :b :c 4]) {:a [1 2 3], :b [], :c [4]})))

(deftest seq-pronunciations-test
  (is (= (take 3 (seq-pronunciations [1])) [[1 1] [2 1] [1 2 1 1]]))
  (is (= (first (seq-pronunciations [1 1 1 4 4])) [3 1 2 4])))
