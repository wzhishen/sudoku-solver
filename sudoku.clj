; Zhishen Wen
; Sept 26, 2013
; CIS 554

(defn replace-row [row]
    (cond (empty? row) []
          (= 0 (first row)) (into [] (cons #{1 2 3 4 5 6 7 8 9} (replace-row (rest row))))
          :else (into [] (cons #{(first row)} (replace-row (rest row)))) ) )

(defn transform [matrix]
    (if (empty? matrix) 
        []
        (into [] (cons (replace-row (first matrix)) (transform (rest matrix)))) ) )

(defn first-three [lst] (take 3 lst))

(defn rest-three [lst] (drop 3 lst))

(defn zip [a b]
    (if (or (empty? a) (empty? b))
        []
        (into [] (cons (into [] (concat (list(first a)) (first b))) (zip (rest a) (rest b)))) ) )

(defn zip-all [a b]
    (if (or (empty? a) (empty? b))
        []
        (into [] (cons (into [] (concat (first a) (first b))) (zip-all (rest a) (rest b)))) ) )

(defn flatten-lst [lst]
    (cond (empty? lst) '()
          (not (set? (first lst))) (concat (flatten-lst (first lst)) (flatten-lst (rest lst)))
          :else (concat (apply list (first lst)) (flatten-lst (rest lst))) ) )

(defn count-all [lst]
    (cond (empty? lst) 0
          (not (coll? (first lst))) (inc (count-all (rest lst))) 
          :else (+ (count-all (first lst)) (count-all (rest lst)))) )

(use 'clojure.set)
(defn get-singletons [lst]
    (cond (empty? lst) #{}
          (not (set? (first lst))) (union (get-singletons (first lst)) (get-singletons (rest lst)))
          (= (count (first lst)) 1) (union (first lst) (get-singletons (rest lst))) 
          :else (union #{} (get-singletons (rest lst))) ) )

(defn member [a lat]
    (cond (empty? lat) false
          (= a (first lat)) true
          :else (member a (rest lat)) ) )

(defn remove-from [lst from-lst]
    (cond (empty? lst) lst
          (member (first lst) from-lst) (remove-from (rest lst) from-lst)
          :else (cons (first lst) (remove-from (rest lst) from-lst)) ) )

(defn remove-num-at [lst singletons]
    (cond (empty? lst) []
          (not (set? (first lst))) (cons (remove-from (remove-num-at (first lst) singletons) singletons) (remove-num-at (rest lst) singletons))
          (> (count (first lst)) 1) (into [] (cons (set (remove-from (first lst) singletons)) (remove-num-at (rest lst) singletons)))
          :else (into [] (cons (first lst) (remove-num-at (rest lst) singletons))) ) )

(defn remove-num-row [matrix]
    (if (empty? matrix)
        []
        (into [] (cons (remove-num-at (first matrix) (get-singletons (first matrix))) (remove-num-row (rest matrix)))) ) )

(defn remove-num-col [matrix]
    (if (empty? (first matrix))
        matrix
        (zip (remove-num-at (map first matrix) (get-singletons (map first matrix))) (remove-num-col (map rest matrix))) ) )

(defn remove-num-box-row [matrix]
    (if (empty? (first matrix))
        matrix
        (zip-all (remove-num-at (map first-three matrix) (get-singletons (map first-three matrix))) (remove-num-box-row (map rest-three matrix))) ) )

(defn remove-num-box [matrix]
    (if (empty? matrix)
        matrix
        (into [] (concat (remove-num-box-row (first-three matrix)) (remove-num-box (rest-three matrix)))) ) )

(defn remove-num [matrix]
    (remove-num-box(remove-num-col(remove-num-row matrix))) )

(defn reduce-set-at [lst freq] 
    (cond (empty? lst) []
          (not-any? (fn [k] (= (get freq k) 1)) (first lst)) (into [] (cons (first lst) (reduce-set-at (rest lst) freq)))
          :else (into [] (cons (set (filter (fn [k] (= (get freq k) 1)) (first lst))) (reduce-set-at (rest lst) freq))) ) )

(defn reduce-set-row [matrix]
    (if (empty? matrix) 
        []
        (into [] (cons (reduce-set-at (first matrix) (frequencies (flatten-lst (first matrix)))) (reduce-set-row (rest matrix)))) ) )

(defn reduce-set-col [matrix]
    (if (empty? (first matrix))
        matrix 
        (zip (reduce-set-at (map first matrix) (frequencies (flatten-lst (map first matrix)))) (reduce-set-col (map rest matrix))) ) )

(defn reduce-set-box-row [matrix]
    (if (empty? (first matrix))
        matrix
        (zip-all (reduce-set-at (map first-three matrix) (frequencies (flatten-lst (map first-three matrix)))) (reduce-set-box-row (map rest-three matrix))) ) )

(defn reduce-set-box [matrix]
    (if (empty? matrix)
        matrix
        (into [] (concat (reduce-set-box-row (first-three matrix)) (reduce-set-box (rest-three matrix)))) ) )

(defn reduce-set [matrix]
    (reduce-set-box(reduce-set-col(reduce-set-row matrix))) )

(defn normalize-row [lst]
    (cond (empty? lst) []
          (not (set? (first lst))) (cons (first lst) (normalize-row (rest lst)))
          (> (count (first lst)) 1) (into [] (cons (first lst) (normalize-row (rest lst))))
          :else (into [] (concat (normalize-row (first lst)) (normalize-row (rest lst)))) ) )

(defn normalize [matrix]
    (if (empty? matrix)
        []
        (into [] (cons (normalize-row (first matrix)) (normalize (rest matrix)))) ) )

(defn process [matrix last-cnt]
    (def ret (reduce-set (remove-num matrix)))
    (if (= (count-all ret) last-cnt)
        ret
        (process ret (count-all ret))) )

(defn solve [matrix]
    (normalize (process (transform matrix) -1)) )