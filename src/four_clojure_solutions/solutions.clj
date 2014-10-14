(ns four-clojure-solutions.infix-calculator)

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
  