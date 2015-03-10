(ns four-clojure-solutions.solutions)

(defn infix [x & args]
  (reduce 
    (fn [operand1 [operator operand2]] 
      (operator operand1 operand2)) 
    x (partition 2 args))) 

; re-implement iterate

(defn my-iterate [f x]
  (cons x (lazy-seq (my-iterate f (f x)))))

; group a sequence

(defn my-group-by [f s]
  (letfn [(get-singleton-maps [f s] 
                              (for [[k v] (partition 2 (interleave (map f s) s))]
                                {k [v]}))]
    (apply merge-with (cons into (get-singleton-maps f s)))))

; pascal's triangle

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
