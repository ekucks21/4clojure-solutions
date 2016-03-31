(ns four-clojure-solutions.solutions)

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
    (into #{} (map (partial into #{}) (vals (filter two-or-more-words? words-by-chars))))))
