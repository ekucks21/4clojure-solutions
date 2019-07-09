(ns four-clojure-solutions.solutions_test
  (:require
   [clojure.test :as t]
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest])
  (:use  [clojure.test])
  (:use  [four-clojure-solutions.solutions]))

(alias 'stc 'clojure.spec.test.check)

;; extracted from clojure.spec.test.alpha
(defn failure-type [x] (::s/failure (ex-data x)))
(defn unwrap-failure [x] (if (failure-type x) (ex-data x) x))

;; modified from clojure.spec.test.alpha
(defn abbrev-result [x]
  (if (-> x :stc/ret :pass?)
    (dissoc x :spec ::stc/ret)
    (-> (dissoc x ::stc/ret)
        (update :spec s/describe)
        (update :failure unwrap-failure))))

(defn throwable? [x]
  (instance? Throwable x))

(defn failure-report [failure]
  (let [abbrev (abbrev-result failure)
        expected (->> abbrev :spec rest (apply hash-map) :ret)
        reason (:failure abbrev)]
    (if (throwable? reason)
      {:type :error
       :message "Exception thrown in check"
       :expected expected
       :actual reason}
      (let [data (ex-data (get-in failure
                                  [::stc/ret
                                   :shrunk
                                   :result-data
                                   :clojure.test.check.properties/error]))]
        {:type     :fail
         :message  (with-out-str (s/explain-out data))
         :expected expected
         :actual   (::s/value data)}))))

(defn check?
  [msg [_ body :as form]]
  `(let [results# ~body
         failures# (remove (comp :pass? ::stc/ret) results#)]
     (if (empty? failures#)
       [{:type    :pass
         :message (str "Generative tests pass for "
                       (str/join ", " (map :sym results#)))}]
       (map failure-report failures#))))

(defmethod t/assert-expr 'check?
  [msg form]
  `(dorun (map t/do-report ~(check? msg form))))

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
  (is (= (k-combinations 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
  (is (= (k-combinations 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"} #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))
  (is (= (k-combinations 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                           #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})))

(deftest prime-sandwich-test
  (is (= false (prime-sandwich 4)))
  (is (= true (prime-sandwich 563)))
  (is (= 1103 (nth (filter prime-sandwich (range)) 15))))

(deftest comp-engine-test
  (is (= 2 ((comp-engine '(/ a b)) '{b 8 a 16})))
  (is (= 8 ((comp-engine '(+ a b 2)) '{a 2 b 4})))
  (is (= [6 0 -4] (map (comp-engine '(* (+ 2 a) (- 10 b))) '[{a 1 b 8} {b 5 a -2} {a 2 b 11}]))))

(deftest sum-multiples-below-test
  ;; (is (= 0 (sum-multiples-below 3 17 11)))
  ;; (is (= 23 (sum-multiples-below 10 3 5)))
  (is (= "2333333316666668" (sum-multiples-below 100000000 3 5))))

(deftest intervals-test
  (is (= [[1 3]] (intervals [1 2 3])))
  (is (= [[1 4] [6 6] [9 11] [13 17] [19 19]]
         (intervals [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])))
  (is (= [[1 3] [8 10]] (intervals [10 9 8 1 2 3]))))

(deftest big-divide-test
  (is (= 23 (big-divide 10 3 5))))

(deftest brackets-balanced-test
  (is (= true (brackets-balanced? "This string has no brackets.")))
  (is (= true (brackets-balanced? "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }")))
  (is (= false (brackets-balanced? "(start, end]"))))

(deftest equivalent-subset-sum-test
  (is (= true (equivalent-subset-sum? #{-1 1 99} 
                                      #{-2 2 888}
                                      #{-3 3 7777})))
  (is (= false (equivalent-subset-sum? #{1}
                                       #{2}
                                       #{3}
                                       #{4})))
  (is (= false (equivalent-subset-sum? #{1 -3 51 9} 
                                       #{0}
                                       #{9 2 81 33}))))

(deftest sequs-horribilis-test
  (is (= '(1 2 (3 (4)))
         (sequs-horribilis 10 [1 2 [3 [4 5] 6] 7])))
  (is (= '(0 1 2 3)
         (sequs-horribilis 9 (range)))))

(deftest trick-winner-test
  (is (= {:suit :club :rank 9}
         ((trick-winner nil) [{:suit :club :rank 4}
                              {:suit :club :rank 9}])))
  (is (= {:suit :club :rank 10} ((trick-winner :club) [{:suit :spade :rank 2}
                                                       {:suit :club :rank 10}])))
  (is (= {:suit :heart :rank 8}
         ((trick-winner :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                                 {:suit :diamond :rank 10} {:suit :heart :rank 4}]))))

(deftest palindromes-test
  (is (= [0 1 2 3 4 5 6 7 8 9 
          11 22 33 44 55 66 77 88 99 
          101 111 121 131 141 151 161]
         (take 26 (palindromes 0))))
  (is (= [171 181 191 202 
          212 222 232 242 
          252 262 272 282 
          292 303 313 323]
         (take 16 (palindromes 162))))
  (is (= [1234554321 1234664321 1234774321 
          1234884321 1234994321 1235005321]
         (take 6 (palindromes 1234550000))))
  (is (= (* 111111111 111111111)
         (first (palindromes (* 111111111 111111111))))))

(deftest infinite-matrix-test
  (is (= [["00" "01" "02" "03" "04" "05"]
          ["10" "11" "12" "13" "14" "15"]
          ["20" "21" "22" "23" "24" "25"]
          ["30" "31" "32" "33" "34" "35"]
          ["40" "41" "42" "43" "44" "45"]]
         (take 5 (map #(take 6 %) (infinite-matrix str)))))
  (is (= [["32" "33" "34" "35" "36"]
          ["42" "43" "44" "45" "46"]
          ["52" "53" "54" "55" "56"]
          ["62" "63" "64" "65" "66"]
          ["72" "73" "74" "75" "76"]
          ["82" "83" "84" "85" "86"]]
         (take 6 (map #(take 5 %) (infinite-matrix str 3 2)))))
  (is (= [[15 18 21 24 27 30 33]
          [20 24 28 32 36 40 44]
          [25 30 35 40 45 50 55]
          [30 36 42 48 54 60 66]
          [35 42 49 56 63 70 77]]
         (infinite-matrix * 3 5 5 7))))

(deftest par-combos-test
  (is (= (nth (sort (par-combos 12)) 5000) "(((((()()()()()))))(()))"))
  (is (= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (par-combos n)) [0 1 2]))))

(deftest longest-consecutive-sub-seq-test
  (is (check? (stest/check `longest-consecutive-sub-seq)))
  (is (= [0 1 2 3] (longest-consecutive-sub-seq [1 0 1 2 3 0 4 5])))
  (is (= [5 6] (longest-consecutive-sub-seq [5 6 1 3 2 7])))
  (is (= [] (longest-consecutive-sub-seq [7 6 5 4]))))

(deftest tic-tac-toe-win-test
  (is (= nil (tic-tac-toe-win [[:e :e :e]
                               [:e :e :e]
                               [:e :e :e]])))
  (is (= :x (tic-tac-toe-win [[:x :e :o]
                              [:x :e :e]
                              [:x :e :o]])))
  (is (= :o (tic-tac-toe-win [[:x :e :o]
                              [:x :o :e]
                              [:o :e :x]]))))

(deftest roman-numerals->digits-test
  (is (= 48 (roman-numerals->digits "XLVIII")))
  (is (= 3999 (roman-numerals->digits "MMMCMXCIX"))))

(deftest triangle-min-path-test
  (is (= 7 (triangle-min-path '([1]
                                [2 4]
                                [5 1 4]
                                [2 3 4 5])))))
