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

(deftest lazy-search-test
  (is (= 3 (lazy-search [3 4 5])))
  (is (= 4 (lazy-search [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
  (is (= 7 (lazy-search (range) (range 0 100 7/6) [2 3 5 7 11 13]))))

(deftest partial-flatten-test
  (is (= (partial-flatten [["Do"] ["Nothing"]]) [["Do"] ["Nothing"]])))

(deftest global-take-while-test
  (is (= [2 3 5 7 11 13] (global-take-while 4
                                            #(= 2 (mod % 3))
                                            [2 3 5 7 11 13 17 19 23])))
  (is (= ["this" "is" "a" "sentence"]
         (global-take-while 3 #(some #{\i} %) ["this" "is" "a" "sentence" "i" "wrote"]))))

(deftest insert-when-test
  (is (= '(1 :less 6 :less 7 4 3) (insert-when < :less [1 6 7 4 3])))
  (is (empty? (insert-when > :more ())))
  (is (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
         (take 12 (->> [0 1]
                       (iterate (fn [[a b]] [b (+ a b)]))
                       (map first)      ; fibonacci numbers
                       (insert-when (fn [a b]    ; both even or both odd
                             (= (mod a 2) (mod b 2)))
                                    :same))))))

(deftest roman-numerals-test
  (is (= "I" (roman-numerals 1)))
  (is (= "XXX" (roman-numerals 30)))
  (is (= "IV" (roman-numerals 4)))
  (is (= "MMMCMXCIX" (roman-numerals 3999))))

(deftest k-combinations-test
  (is (= (k-combinations 1 #{4 5 6}) #{#{4} #{5} #{6}}))
  (is (= (k-combinations 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                           #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})))
