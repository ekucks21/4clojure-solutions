(ns four-clojure-solutions.solutions
  (:require [clojure.set :as s]
            [com.rpl.specter :refer [srange filterer transform]]))

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
     (count (reduce clojure.set/union sets)))
  ) 

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
      (s/union subsets-without-first (map #(conj % (first s)) subsets-without-first)))))

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
  (let [sorted-contains? (fn [x coll] (some (partial = x) (filter (partial >= x) coll)))
        every-contains? (fn [x xs] (every? #(sorted-contains? x %) xs))]
    (first (filter #(every-contains? % rest-xs) xs))))
