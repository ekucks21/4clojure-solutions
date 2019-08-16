(ns four-clojure-solutions.solutions
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.data :as data]
            [clojure.test.check.generators :as gen]
            [com.rpl.specter :refer [filterer srange transform]]
            [orchestra.spec.test :as st]))

(defn infix [x & args]
  (reduce
    (fn [operand1 [operator operand2]]
      (operator operand1 operand2))
    (partition 2 args)))

; re-implement iterate

(defn my-iterate [f x]
  (cons x (lazy-seq (my-iterate f (f x)))))

; group a sequence

(defn my-group-by [f s]
  (letfn [(get-singleton-maps [f s]
            (for [[k v] (partition 2 (interleave (map f s) s))]
              {k [v]}))]
    (apply merge-with (cons into (get-singleton-maps f s)))))
;
;; pascal's triangle

(defn tree-to-table [amap]
  (for [parent-key (keys amap)
        child-key (keys (parent-key amap))]
    [[parent-key child-key] (get-in amap [parent-key child-key])]))

(defn insert [n alon]
  (cond
    (empty? alon) (cons n nil)
    (<= n (first alon)) (cons n alon)
    (> n (first alon)) (cons (first alon) (insert n (rest alon)))))

(defn isort [alon]
  (loop [coll alon]
    (cond
      (empty? alon) nil
      (seq? alon) (insert (first alon) (isort (rest alon))))))

(defn split-with-vec [pred vec]
  (let [splitIndex (first (keep-indexed #(if (not (pred %2)) %1) vec))]
    (if splitIndex
      [(subvec vec 0 splitIndex) (subvec vec splitIndex)]
      [vec []])))

(defn insert-into [s x]
  (let [[low high] (split-with-vec #(< % x) s)]
    (-> low
        (into [x])
        (into high))))

(defn insertion-sort [s]
  (reduce insert-into [] s))

(defn append [l1 l2]
  (loop [l1 (seq l1) result l2]
    (if l1
      (recur (next l1) (cons (first l1) result))
      result)))

(defn insert [n alon]
  (loop [alon (seq alon), traversed nil]
    (cond
      (nil? alon) (append traversed (list n))
      (<= n (first alon)) (append traversed (cons n alon))
      (> n (first alon)) (recur (next alon) (conj traversed (first alon))))))

(defn isort [alon]
  (loop [alon (seq (reverse alon)), sorted nil]
    (if alon
      (recur (next alon) (insert (first alon) sorted))
      sorted)))

(defn bad-insert-sort [coll]
  (loop [sortedIndex 0 beingSorted coll]
    (let [currentItem (beingSorted sortedIndex)
          collWithoutCurrent (dissoc beingSorted sortedIndex)
          reversedSubSection (rseq (first (split-at sortedIndex beingSorted)))
          indexToMoveBefore (first (keep-indexed #(if (> %2 currentItem) %1) reversedSubSection))
          splitColl (split-at indexToMoveBefore collWithoutCurrent)]
      (recur (inc sortedIndex)
             (concat (first splitColl) currentItem (first splitColl))))))

(defn get-pascal-row [row]
  (letfn [(calculate-row [prevRow]
            (flatten [1
                      (map (fn [[x y]] (+ x y)) (partition 2 1 prevRow))
                      1]))]
    (nth (iterate calculate-row [1]) (dec row))))

(defn disjoint? [sets]
  (= (apply + (map count sets))
     (count (reduce clojure.set/union sets))))


(defn symmetric-tree? [tree]
  (letfn [(reverse-tree [[parent left right]]
            [parent
             (if (coll? right) (reverse-tree right) right)
             (if (coll? left) (reverse-tree left) left)])]
    (= (second tree)
       (reverse-tree (nth tree 2)))))

(defn count-nodes [tree]
  (letfn [(count-children [tree] (let [child-count (map #(if (coll? %) (count-children %) 0) tree)
                                       child-and-parent-count (map inc child-count)]
                                   (reduce + child-and-parent-count)))]
    (inc (count-children tree))))

(defn seq-type? [aseq]
  (letfn [(is-set? [aseq] (let [orig-count (count aseq)
                                conjed (conj aseq 1 1)]
                            (= (inc orig-count) (count conjed))))
          (is-vector? [aseq] (let [unique-marker1 (gensym)
                                   unique-marker2 (gensym)
                                   conjed (conj aseq unique-marker1 unique-marker2)]
                               (condp = unique-marker2
                                 (first conjed) false
                                 (last conjed) true)))
          (is-list? [aseq] (let [unique-marker1 (gensym)
                                 unique-marker2 (gensym)
                                 conjed (conj aseq unique-marker1 unique-marker2)]
                             (condp = unique-marker2
                               (first conjed) true
                               (last conjed) false)))
          (is-map? [aseq] (let [unique-marker (gensym)
                                pairs-to-conj {:whatever 1 unique-marker 2}
                                conjed (conj aseq pairs-to-conj)]
                            (not (or (= pairs-to-conj (first conjed))
                                     (= pairs-to-conj (last conjed))))))]
    (cond
      (is-map? aseq) :map
      (is-set? aseq) :set
      (is-list? aseq) :list
      (is-vector? aseq) :vector)))

(defn find-anagrams [avec]
  (let [words-by-chars (group-by sort avec)
        two-or-more-words? (comp (partial < 1) count val)]
    (set (map set (vals (filter two-or-more-words? words-by-chars))))))

(defn reduction-steps
  ([f aseq] (reduction-steps f (first aseq) (rest aseq)))
  ([f value aseq]
   (lazy-seq (if (first aseq)
               (cons value (reduction-steps f (f value (first aseq)) (rest aseq)))
               (cons value aseq)))))

(defn perfect? [x]
  (let [divisors (filter #(= 0 (mod x %)) (rest (range (+ 1 (/ x 2)))))]
    (= x (apply + divisors))))

(defn into-camel-case [s]
  (clojure.string/replace s #"-([a-z])" #(clojure.string/upper-case (second %1))))

(defn my-merge-with [f first-map & maps]
  (letfn [(merge-map [m1 m2] (reduce merge-entry m1 m2))
          (merge-entry [m [key value]]
            (if (get m key)
              (assoc m key (f (get m key) value))
              (assoc m key value)))]
    (reduce merge-map first-map maps)))

(defn reverse-even-in-range [coll x y]
  (let [head (take x coll)
        sub-range (subvec coll x y)
        tail (subvec coll y)
        even-odd (group-by even? sub-range)
        even-reverse (reverse (even-odd true))
        new-sub-range (let [[smaller larger] (if (> (count even-reverse) (count (even-odd false)))
                                               [(even-odd false) even-reverse]
                                               [even-reverse (even-odd false)])
                            remainder (take-last (- (count larger) (count smaller)) larger)
                            interleaved (if (even? (first sub-range))
                                          (interleave even-reverse (even-odd false))
                                          (interleave (even-odd false) even-reverse))]
                        (concat interleaved remainder))]
    (vec (concat head new-sub-range tail))))

(defn specter-reverse-even-in-range [coll x y]
  (transform [(srange x y) (filterer even?)] reverse coll))

(defn cond-inc [coll]
  (reduce (fn [new-coll amap]
            (conj new-coll (if (even? (amap :a))
                             (update amap :c (comp vec (partial map inc)))
                             (update amap :d inc))))
          [] coll))

(defn totient [x]
  (case x
    1 1
    (letfn [(greatest-common-divisor [x y]
              (let [[smaller larger] (sort [x y])
                    candidates (reverse (range 1 (inc smaller)))]
                (first (filter #(every? identity (map (comp (fn [num] (zero? num))
                                                            (fn [num] (rem num %))) [x y]))
                               candidates))))]
      (count (filter (comp (partial = 1) (partial greatest-common-divisor x)) (range 1 x))))))

(defn happy-number? [x]
  (let [digits (fn [y] (->> y
                            (iterate #(quot % 10))
                            (take-while pos?)
                            (mapv #(mod % 10))
                            rseq))
        square-sums (iterate (fn [coll] (apply + (map #(int (Math/pow % 2)) (digits coll)))) x)]
    (= 1 (first (filter #(= 1 %) (take 10000 square-sums))))))

(defn my-trampoline [f & args]
  (first (drop-while fn? (iterate #(%) (apply f args)))))

(defn balanced? [n]
  (let [digits (fn [y] (->> y
                            (iterate #(quot % 10))
                            (take-while pos?)
                            (mapv #(mod % 10))
                            rseq))
        n-digits (digits n)
        digits-count (int (Math/ceil (/ (count n-digits) 2)))]
    (= (apply + (take digits-count n-digits))
       (apply + (take-last digits-count n-digits)))))

(defn power-set [s]
  (case s
    #{} #{#{}}
    (let [subsets-without-first (power-set (disj s (first s)))]
      (set/union subsets-without-first (map #(conj % (first s)) subsets-without-first)))))

(defn equivalance-classes [f s]
  (set (map set (vals (group-by f s)))))

(defn keys-and-values [s]
  (first (reduce (fn [[m current-key] item]
                   (let [v (if (keyword? item) [] [item])
                         next-key (if (keyword? item) item current-key)]
                     [(merge-with (comp vec concat) m {next-key v})
                      next-key]))
                 [{}] s)))

(defn digits-and-bases [x base]
  (if (zero? x)
    [0]
    (->> x
         (iterate #(quot % base))
         (take-while (complement zero?))
         (mapv #(mod % base))
         rseq)))

(defn oscilrate [x f & rest-fns]
  (let [result (f x)]
    (lazy-seq (cons x (apply oscilrate result (conj (vec rest-fns) f))))))

(defn seq-pronunciations [xs]
  (letfn [(pronunciation [nums]
            (mapcat (juxt count first) (partition-by identity nums)))]
    (rest (iterate pronunciation xs))))

(defn decurry [f]
  (fn [& args] (reduce #(% %2) f args)))

(defn lazy-search [xs & rest-xs]
  (let [sorted-contains? (fn [x coll] (some (partial = x) (take-while (partial >= x) coll)))
        every-contains? (fn [x xs] (every? #(sorted-contains? x %) xs))]
    (first (filter #(every-contains? % rest-xs) xs))))

(defn partial-flatten [& nested]
  (filter #(and (coll? %) (->> %
                               first
                               coll?
                               not))
          (tree-seq coll? identity nested)))

(defn global-take-while [n p xs]
  (letfn [(step [num-matched [x & more]]
            (let [new-num-matched (if (p x) (inc num-matched) num-matched)]
              (if (= new-num-matched n)
                nil
                (lazy-seq (cons x (step new-num-matched more))))))]
    (step 0 xs)))

(defn insert-when [p match-value [x y & more :as xs]]
  (if (empty? xs) xs
                  (lazy-seq (concat
                              (if (and y (p x y)) [x match-value] [x])
                              (insert-when p match-value (rest xs))))))

(defn roman-numerals [x]
  (let [num-to-roman {1 "I", 5 "V", 10 "X", 50 "L", 100 "C", 500 "D", 1000 "M"}
        place->roman (fn [[place z]] (let [one-symbol (num-to-roman place)
                                           five-symbol (num-to-roman (* place 5))
                                           ten-symbol (num-to-roman (* place 10))]
                                       (case z
                                         0 ""
                                         1 one-symbol
                                         2 (str one-symbol one-symbol)
                                         3 (str one-symbol one-symbol one-symbol)
                                         4 (str one-symbol five-symbol)
                                         5 (str five-symbol)
                                         6 (str five-symbol one-symbol)
                                         7 (str five-symbol one-symbol one-symbol)
                                         8 (str five-symbol one-symbol one-symbol one-symbol)
                                         9 (str one-symbol ten-symbol))))]
    (->> x
         (iterate #(quot % 10))
         (take-while (partial not= 0))
         (map #(mod % 10))
         (map-indexed (fn [i y] [(int (Math/pow 10 i)) y]))
         (vec)
         (rseq)
         (map place->roman)
         (apply str))))

(defn k-combinations [subset-size xs]
  (let [xs-vec (vec xs)
        increment-index (fn [indexes]
                          (let [[before-inc-index [index-to-inc & more]]
                                (map (partial map first)
                                     (split-with
                                       (fn [[i1 i2]] (not (< i1 (dec i2))))
                                       (partition 2 1 [(+ 2 (last indexes))] indexes)))]
                            (concat (range (count before-inc-index))
                                    [(inc index-to-inc)]
                                    more)))
        k-combo-indexes (->> subset-size
                             range
                             (iterate increment-index)
                             (take-while
                               (partial not-any? (partial <= (count xs)))))]
    (into #{} (map (fn [indexes] (into #{} (map (partial xs-vec) indexes))) k-combo-indexes))))

(defn prime-sandwich [n]
  (let [primes (fn primes [x primes-found]
                 (lazy-seq
                   (if (not-any? (comp (partial = 0) (partial mod x)) primes-found)
                     (cons x (primes (inc x) (conj primes-found x)))
                     (primes (inc x) primes-found))))]
    (if-let [[before _ after] (->> (primes 2 [])
                                   (partition 3 1)
                                   (take-while (comp (partial >= n) second))
                                   (filter (comp (partial = n) second))
                                   first)]
      (if (= n (/ (+ before after) 2)) true false)
      false)))

(defn comp-engine [f]
  (fn [m] ((fn compute [[f-sym & args]]
             (let [resolved-f (condp = f-sym
                                '/ /
                                '+ +
                                '- -
                                '* *)
                   resolved-args (map #(cond
                                         (coll? %) (compute %)
                                         (symbol? %) (% m)
                                         :else %) args)]
               (apply resolved-f resolved-args))) f)))

(defn sum-multiples-below [n a b]
  (let [frobensius-num (- (* a b) (+ a b))
        exp-combos (iterate (fn [[exp1 exp2]]
                              (cond
                                (= exp1 exp2) [(inc exp1) 0]
                                (= exp1 (inc exp2)) [0 (inc exp2)]
                                (> exp2 exp1) [(inc exp1) exp2]
                                (> exp1 exp2) [exp1 (inc exp2)])) [1 0])
        representables (->> exp-combos
                            (map (fn [exps]
                                   (map (fn [num exp]
                                          (if (= exp 0)
                                            0
                                            (.pow (BigInteger. (str num)) exp)))
                                        [a b] exps)))
                            (map (partial apply +)))
        representable? (fn [x] (->> representables
                                    (take-while (partial > x))
                                    (some (partial = x))
                                    some?))
        representables-below-frobensius (->> frobensius-num
                                             (range (min a b))
                                             (filter representable?))]
    (apply + (concat representables-below-frobensius (range (inc frobensius-num) n)))))

(defn intervals [xs]
  (if (empty? xs) []
                  (let [sorted-xs (distinct (sort xs))
                        partitioned (reduce #(let [last-number (last (last %1))
                                                   interval (- %2 last-number)]
                                               (if (> interval 1) (conj %1 [%2]) (update-in %1 [(dec (count %1))] conj %2)))
                                            [[(first sorted-xs)]] (rest sorted-xs))]
                    (map (fn [[x & more]] (if (nil? more) [x x] [x (last more)])) partitioned))))

(defn big-divide [n a b]
  (let [n-1 (dec n)
        series (fn [interval x]
                 (let [n-terms (quot x interval)
                       last-dividend (* interval n-terms)]
                   (* (/ n-terms 2) (+ interval last-dividend))))
        a-series (series a n-1)
        b-series (series b n-1)
        lcm-series (series (* a b) n-1)]
    (- (+ a-series b-series) lcm-series)))

(defn brackets-balanced? [s]
  (let [is-opener? #(re-matches #"[\[\{\(]" (str %1))
        is-closer? #(re-matches #"[\]\}\)]" (str %1))
        get-matching-bracket #(case %1
                                \[ \]
                                \] \[
                                \{ \}
                                \} \{
                                \( \)
                                \) \(
                                nil)]
    (empty? (reduce #(cond
                       (is-opener? %2) (conj %1 %2)
                       (is-closer? %2) (if (= (last %1) (get-matching-bracket %2))
                                         (into [] (butlast %1))
                                         (conj %1 %2))
                       :else %1)
                    [] s))))

(defn equivalent-subset-sum? [& int-sets]
  (let [convert-to-binary (fn [x]
                            (if (zero? x)
                              [0]
                              (->> x
                                   (iterate #(quot % 2))
                                   (take-while (complement zero?))
                                   (mapv #(mod % 2))
                                   rseq)))
        num-sub-sets (fn [set-size]
                       (dec (reduce * (repeat set-size 2))))
        set-sums (let [sums (fn [int-set]
                              (let [subset-masks
                                    (map convert-to-binary
                                         (range 1 (inc (num-sub-sets (count int-set)))))]
                                (map
                                  (fn [subset-mask]
                                    (->> subset-mask
                                         (map vector int-set)
                                         (filter #(= 1 (second %1)))
                                         (map first)
                                         (reduce +)))
                                  subset-masks)))]
                   (map sums int-sets))]
    (not (empty? (apply clojure.set/intersection (map set set-sums))))))

(defn sequs-horribilis
  ([x xs] (second (sequs-horribilis x xs 0)))
  ([x xs sum]
   (->> xs
        (reductions (fn [[r-sum _] n]
                      (if (coll? n)
                        (let [[sub-seq-sum sub-seq] (sequs-horribilis x n r-sum)]
                          [sub-seq-sum sub-seq])
                        [(+ n r-sum) n]))
                    [sum nil])
        rest
        (take-while (comp (partial >= x) first))
        ((juxt (comp first last) (partial map second))))))

(defn trick-winner [trump]
  (fn [cards]
    (let [suit-order (->> [(:suit (first cards)) trump]
                          distinct
                          (map-indexed (comp reverse vector))
                          flatten
                          (apply hash-map))]
      (last (sort-by (juxt (comp suit-order :suit) :rank) cards)))))

(defn palindromes [start]
  (let [num->digits (fn [x]
                      (if (= x 0)
                        '(0)
                        (->> x
                             vector
                             (iterate (comp (juxt #(quot % 10) #(mod % 10)) first))
                             rest
                             (take-while (partial some (partial < 0)))
                             (map second)
                             reverse)))
        digits->num #(->> %
                          reverse
                          (map * (iterate (partial * 10) 1))
                          (apply +))
        palindrome? #(let [digits (num->digits %)
                           mirror-size (quot (count digits) 2)
                           left (take mirror-size digits)
                           rev-right (take mirror-size (reverse digits))]
                       (or (= left rev-right) (= 1 (count digits))))
        palindrome-seq (rest
                        (iterate
                         (fn [x]
                           (let [digits (num->digits x)
                                 digits-count (count digits)
                                 left-size (if (odd? digits-count)
                                             (inc (quot digits-count 2))
                                             (quot digits-count 2))
                                 [left-mirror right-mirror] (split-at left-size digits)
                                 inc-fragment (vec (drop-while (partial = 9) (reverse left-mirror)))
                                 inc-left-mirror (if (not (empty? inc-fragment))
                                                   (take (count left-mirror)
                                                         (concat
                                                          (reverse
                                                           (assoc inc-fragment 0
                                                                  (inc (first inc-fragment))))
                                                          (repeat 0))))
                                 palindrome-digits (if (and (even? digits-count)
                                                            (< (digits->num (reverse right-mirror))
                                                               (digits->num left-mirror)))
                                                     (concat left-mirror (reverse left-mirror))
                                                     (if (not (empty? inc-fragment))
                                                       (if (= 1 digits-count)
                                                         inc-left-mirror
                                                         (concat inc-left-mirror
                                                                 (reverse
                                                                  (take (quot digits-count 2)
                                                                        inc-left-mirror))))
                                                       (concat [1] (repeat (- digits-count 1) 0) [1])))
                                 palindrome-num (digits->num palindrome-digits)]
                             (do (println "palindrome-num: " palindrome-num)
                                 (println "left-mirror: " left-mirror)
                                 (println "inc-fragment: " inc-fragment)
                                 (println "inc-left-mirror: " inc-left-mirror)
                                 palindrome-num)))
                         start))]
    (if (palindrome? start) (conj palindrome-seq start) palindrome-seq)))

(defn infinite-matrix
  ([f] (infinite-matrix f 0 0))
  ([f m n] (let [columns (fn columns [column]
                           (lazy-seq (cons (f m (+ n column)) (columns (inc column)))))]
             (lazy-seq (cons (columns 0) (infinite-matrix f (inc m) n)))))
  ([f m n s t] (map (partial take t) (take s (infinite-matrix f m n)))))

(defn par-combos 
  ([n] (par-combos n n "" #{}))
  ([left-pars right-pars par-combo par-combo-set]
   (if (= 0 left-pars right-pars)
     (conj par-combo-set par-combo)
     (-> par-combo-set
         (#(if (> right-pars left-pars)
             (into % (par-combos
                      left-pars
                      (dec right-pars)
                      (str par-combo ")")
                      par-combo-set))
             %))
         (#(if (> left-pars 0)
             (into % (par-combos
                      (dec left-pars)
                      right-pars
                      (str par-combo "(")
                      par-combo-set))
             %))))))

(defn all-paren [n]
  (do
    (println "all-paren: " n)
    (if (== n 0) #{""}
        (into #{} (for [i (range n) j (all-paren i) k (all-paren (- n 1 i))]
                    (do
                      (println (str "i: " i " j: " j " k: " k " result: "
                                    (str "(" j ")" k)))
                      (str "(" j ")" k)))))))

(defn longest-consecutive-sub-seq [xs]
  (->> xs
       (partition-all 2 1)
       (map #(hash-map :diff (apply - %) :pair %))
       (partition-by :diff)
       (filter (partial every? (comp (partial = -1) :diff)))
       (map (partial map :pair))
       (map #(conj (map second %) (ffirst %)))
       (map (partial filter (complement nil?)))
       (apply max-key count [])))

(s/def ::addable-int (s/with-gen int? #(gen/large-integer* {:min (- (quot Long/MAX_VALUE 2))
                                                         :max (quot Long/MAX_VALUE 2)})))

(s/def ::java-int (s/with-gen int? #(gen/large-integer* {:min (- Integer/MAX_VALUE)
                                                         :max Integer/MAX_VALUE})))

(defn subtract [x y]
  (- x y))

(s/fdef subtract
  :args (s/cat :x ::addable-int :y ::addable-int))

(s/fdef longest-consecutive-sub-seq
  :args (s/cat :xs (s/coll-of ::addable-int))
  :ret (s/coll-of int?)
  :fn (s/and #(<= (count (:ret %)) (count (-> % :args :xs)))
             (fn [{ret :ret {xs :xs} :args}]
                (if (empty? xs)
                  true
                  (some (partial = ret) (partition (count ret) 1 xs))))
             #(let [first-ret (or (first (:ret %)) 0)]
                (= (:ret %) (range first-ret (+ (count (:ret %)) first-ret))))))

(defn tic-tac-toe-win [board]
  (let [horizontal-winner (ffirst (filter (comp (partial = 1) count) (map distinct board)))
        vertical-winner (ffirst (filter
                                 (comp (partial = 1) count)
                                 (apply map (fn [& bar] (distinct bar)) board)))
        diagonal-winner (ffirst (filter (comp (partial = 1) count)
                                        (map (comp
                                              distinct
                                              (partial map-indexed (fn [i row] (nth row i))))
                                             [board (rseq board)])))]
    (first (filter #{:x :o} [horizontal-winner vertical-winner diagonal-winner]))))

(defn roman-numerals->digits [roman-numerals]
  (let [roman-numeral->digit {\M 1000
                              \D 500
                              \C 100
                              \L 50
                              \X 10
                              \V 5
                              \I 1}
        raw-digits (map roman-numeral->digit roman-numerals)
        subtractions (->> raw-digits
                          (partition 2 1)
                          (filter (partial apply <))
                          (map (comp - (partial * 2) first)))]
    (+ (apply + raw-digits) (apply + subtractions))))

(defn triangle-min-path [triangle]
  (let [min-path-search (fn min-path-search [[row & next-triangle-section] sum i lowest-sum]
                          (cond
                            (nil? row) sum
                            (and lowest-sum (> sum lowest-sum)) lowest-sum
                            :else (let [[left-sum right-sum :as sums] (map (partial + sum) (subvec row i (+ i 2)))]
                                    (if next-triangle-section
                                      (->> lowest-sum
                                           (min-path-search next-triangle-section left-sum i)
                                           (min-path-search next-triangle-section right-sum (inc i)))
                                      (first (sort (filter (complement nil?) (conj sums lowest-sum))))))))]
    (min-path-search (rest triangle) (ffirst triangle) 0 nil)))

(s/def ::triangle (s/with-gen
                    (s/and (s/coll-of (s/coll-of ::java-int))
                           #(let [first-count (count (first %))]
                              (= (range first-count (+ first-count (count %)))
                                 (map count %))))
                    (fn [] (gen/let [size gen/small-integer]
                             (apply gen/tuple
                                    (map
                                     #(gen/vector (s/gen ::java-int) %)
                                     (range 1 (inc size))))))))

(s/fdef triangle-min-path
  :args (s/cat :triangle ::triangle)
  :ret (s/nilable int?)
  :fn (fn [{ret :ret {triangle :triangle} :args}]
         (if ret
           (<= ret (reduce + (map #(Math/abs %) (flatten triangle))))
           (empty? (flatten triangle)))))

(defn transitive-closure [binary-relations]
  (let [right-by-left (reduce-kv
                       (fn [m k v] (assoc m k (map second v)))
                       {}
                       (group-by first binary-relations))
        next-relations (fn next-relations [relations first-level-relations]
                         (if (empty? first-level-relations)
                           relations
                           (next-relations
                            (into relations first-level-relations)
                            (clojure.set/difference
                             (into #{} (flatten
                                        (remove nil? (map right-by-left first-level-relations))))
                             relations))))
        transitive-right-by-left (reduce-kv
                                  (fn [m k v] (assoc m k (next-relations #{} v)))
                                  {} right-by-left)]
    (reduce-kv (fn [s k v] (into s (map (partial vector k) v))) #{} transitive-right-by-left)))

(s/def ::not-coll (s/or :b boolean? :s string? :sym symbol? :n number?))

(s/def ::binary-relations (s/with-gen
                            (s/coll-of (s/coll-of ::not-coll :kind vector? :count 2) :kind set?)
                            #(gen/let [elements (gen/such-that (complement empty?)
                                                               (gen/vector (s/gen ::not-coll)))]
                               (gen/set (gen/vector (gen/elements elements) 2)))))

(s/fdef transitive-closure
  :args (s/cat :binary-relations ::binary-relations)
  :ret ::binary-relations
  :fn (fn [{ret :ret {binary-relations :binary-relations} :args}]
        #(empty? (set/difference binary-relations ret))))

(defn word-chain? [xs]
  (let [one-char-diff? (fn [[word1 word2]]
                         (let [[only-left only-right] (map (comp (partial apply +) vals)
                                                           (data/diff (frequencies word1)
                                                                      (frequencies word2)))]
                           (and (<= 0 only-left 1)
                                (<= 0 only-right 1)
                                (< 0 (+ only-left only-right) 3))))
        one-char-diffs (->> xs
                            (iterate rest)
                            (take-while (comp (partial < 1) count))
                            (mapcat #(map (partial vector (first %)) (rest %)))
                            (filter one-char-diff?))
        adjacency-list (merge-with set/union
                                   (reduce-kv (fn [m k v] (assoc m k (into #{} (map second v))))
                                              {}
                                              (group-by first one-char-diffs))
                                   (reduce-kv (fn [m k v] (assoc m k (into #{} (map first v))))
                                              {}
                                              (group-by second one-char-diffs)))
        chain-from? (fn chain-from? [words-visited adjacent]
                      (cond
                        (= (count xs) (count words-visited)) true
                        (empty? adjacent) false
                        :else (some true?
                                    (map #(chain-from?
                                           (conj words-visited %)
                                           (set/difference (adjacency-list %) words-visited))
                                         adjacent))))]
    (= true (some true? (map #(chain-from? #{%} (adjacency-list %)) xs)))))

(s/fdef word-chain?
  :args (s/cat :words (s/coll-of string? :kind set?))
  :ret boolean?)

(st/instrument)

