; Zhishen Wen
; Sept 28, 2013
; CIS 554

(ns user (:use clojure.test))

(deftest test-replace-row
    (is (= '[#{1 2 3 4 5 6 7 8 9} #{1} #{2}] (replace-row '[0 1 2])))
    (is (= '[#{1} #{2}] (replace-row '[1 2])))
    (is (= '[#{1 2 3 4 5 6 7 8 9}] (replace-row '[0])))
)

(deftest test-transform
    (is (= '[[#{1 2 3 4 5 6 7 8 9} #{2} #{5} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9}] 
    [#{1} #{1 2 3 4 5 6 7 8 9} #{4} #{2} #{5} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9}] 
    [#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{4} #{2} #{1} #{1 2 3 4 5 6 7 8 9}] 
    [#{1 2 3 4 5 6 7 8 9} #{5} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{3} #{2} #{1 2 3 4 5 6 7 8 9}] 
    [#{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{2} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{9}] 
    [#{1 2 3 4 5 6 7 8 9} #{8} #{7} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9}] 
    [#{1 2 3 4 5 6 7 8 9} #{9} #{1} #{5} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9}] 
    [#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{7} #{8} #{1} #{1 2 3 4 5 6 7 8 9} #{3}] 
    [#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]] 
    (transform 
    '[[0 2 5 0 0 1 0 0 0]
      [1 0 4 2 5 0 0 0 0]
      [0 0 6 0 0 4 2 1 0]
      [0 5 0 0 0 0 3 2 0]
      [6 0 0 0 2 0 0 0 9]
      [0 8 7 0 0 0 0 6 0]
      [0 9 1 5 0 0 6 0 0]
      [0 0 0 0 7 8 1 0 3]
      [0 0 0 6 0 0 5 9 0]])))
)

(deftest test-first-three
  (is (= '(1 2 3) (first-three '(1 2 3 4 5))))
  (is (= '(1 2) (first-three '(1 2))))
)

(deftest test-rest-three
  (is (= '(4 5) (rest-three '(1 2 3 4 5))))
  (is (= '() (rest-three '(1 2))))
)

(deftest test-zip
  (is (= '[[[1] 1] [[2] 2]] (zip '[[1] [2]] '[[1] [2]])))
)

(deftest test-zip-all
  (is (= '[[1 1] [2 2]] (zip-all '[[1] [2]] '[[1] [2]])))
)

(deftest test-flatten-lst
  (is (= '(1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 6 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 5 9 1 2 3 4 5 6 7 8 9)
         (flatten-lst 
         '[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}])))
)

(deftest test-count-all
  (is (= 57
         (count-all
         '[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}])))
)

(deftest test-get-singletons
  (is (= #{5 6 9}
         (get-singletons
         '[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}])))
)

(deftest test-member
  (is (member #{1 2 3 4 5 6 7 8 9} 
      '[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]))
)

(deftest test-remove-from
  (is (= '(6 7 8 9) (remove-from '[ 1 2 3 4 5 6 7 8 9] #{1 2 3 4 5} )))
)

(deftest test-remove-num-at
  (is (= '[#{1 2 3 4 7 8} #{1 2 3 4 7 8} #{1 2 3 4 7 8} #{6} #{1 2 3 4 7 8} #{1 2 3 4 7 8} #{5} #{9} #{1 2 3 4 7 8}]
         (remove-num-at '[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]
                  #{5 6 9})))
)

(deftest test-remove-num-row
  (is (= '[[#{1 2 3 4 7 8} #{1 2 3 4 7 8} #{1 2 3 4 7 8} #{6} #{1 2 3 4 7 8} #{1 2 3 4 7 8} #{5} #{9} #{1 2 3 4 7 8}]]
         (remove-num-row '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]])))
)

(deftest test-remove-num-col
  (is (= '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]]
         (remove-num-col '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]])))
)

(deftest test-remove-num-box-row
  (is (= '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 7 8 9} #{1 2 3 4 5 7 8 9} #{5} #{9} #{1 2 3 4 6 7 8}]]
         (remove-num-box-row '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]])))
)

(deftest test-remove-num-box
  (is (= '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 7 8 9} #{1 2 3 4 5 7 8 9} #{5} #{9} #{1 2 3 4 6 7 8}]]
         (remove-num-box '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]])))
)

(deftest test-remove-num
  (is (= '[[#{1 2 3 4 7 8} #{1 2 3 4 7 8} #{1 2 3 4 7 8} #{6} #{1 2 3 4 7 8} #{1 2 3 4 7 8} #{5} #{9} #{1 2 3 4 7 8}]]
         (remove-num '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]])))
)

(deftest test-reduce-set-at
  (is (= '[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]
         (reduce-set-at '[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]
                     (frequencies (flatten-lst ['#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}])))))
)

(deftest test-reduce-set-row
  (is (= '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]]
         (reduce-set-row '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]])))
)

(deftest test-reduce-set-col
  (is (= '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]]
         (reduce-set-col '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]])))
)

(deftest test-reduce-set-box-row
  (is (= '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]]
         (reduce-set-box-row '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]])))
)

(deftest test-reduce-set-box
  (is (= '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]]
         (reduce-set-box '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]])))
)

(deftest test-reduce-set
  (is (= '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]]
         (reduce-set '[[#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]])))
)

(deftest test-normalize-row
  (is (= '(#{1 2} 3 4) (normalize-row '[#{1 2} #{3} #{4}])))
  (is (= '(1 3 4) (normalize-row '[#{1} #{3} #{4}])))
  (is (= '[] (normalize-row '[])))
)

(deftest test-normalize
  (is (= '[[#{1 2} 3 4]] (normalize '[[#{1 2} #{3} #{4}]])))
  (is (= '[[1 3 4]] (normalize '[[#{1} #{3} #{4}]])))
  (is (= '[[]] (normalize '[[]])))
)

(deftest test-process
  (is (= '[[#{8} #{2} #{5} #{7} #{6} #{1} #{9} #{3} #{4}] 
    [#{1} #{3} #{4} #{2} #{5} #{9} #{7} #{8} #{6}] 
    [#{9} #{7} #{6} #{8} #{3} #{4} #{2} #{1} #{5}] 
    [#{4} #{5} #{9} #{1} #{8} #{6} #{3} #{2} #{7}] 
    [#{6} #{1} #{3} #{4} #{2} #{7} #{8} #{5} #{9}] 
    [#{2} #{8} #{7} #{3} #{9} #{5} #{4} #{6} #{1}] 
    [#{3} #{9} #{1} #{5} #{4} #{2} #{6} #{7} #{8}] 
    [#{5} #{6} #{2} #{9} #{7} #{8} #{1} #{4} #{3}] 
    [#{7} #{4} #{8} #{6} #{1} #{3} #{5} #{9} #{2}]] 
    (process '[[#{1 2 3 4 5 6 7 8 9} #{2} #{5} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9}] 
    [#{1} #{1 2 3 4 5 6 7 8 9} #{4} #{2} #{5} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9}] 
    [#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{4} #{2} #{1} #{1 2 3 4 5 6 7 8 9}] 
    [#{1 2 3 4 5 6 7 8 9} #{5} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{3} #{2} #{1 2 3 4 5 6 7 8 9}] 
    [#{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{2} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{9}] 
    [#{1 2 3 4 5 6 7 8 9} #{8} #{7} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9}] 
    [#{1 2 3 4 5 6 7 8 9} #{9} #{1} #{5} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9}] 
    [#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{7} #{8} #{1} #{1 2 3 4 5 6 7 8 9} #{3}] 
    [#{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{6} #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9} #{5} #{9} #{1 2 3 4 5 6 7 8 9}]] 0)))
)

(deftest test-solve
  (is (= '[[8 2 5 7 6 1 9 3 4] 
           [1 3 4 2 5 9 7 8 6] 
           [9 7 6 8 3 4 2 1 5] 
           [4 5 9 1 8 6 3 2 7] 
           [6 1 3 4 2 7 8 5 9] 
           [2 8 7 3 9 5 4 6 1] 
           [3 9 1 5 4 2 6 7 8] 
           [5 6 2 9 7 8 1 4 3] 
           [7 4 8 6 1 3 5 9 2]]
         (solve 
         '[[0 2 5 0 0 1 0 0 0]
           [1 0 4 2 5 0 0 0 0]
           [0 0 6 0 0 4 2 1 0]
           [0 5 0 0 0 0 3 2 0]
           [6 0 0 0 2 0 0 0 9]
           [0 8 7 0 0 0 0 6 0]
           [0 9 1 5 0 0 6 0 0]
           [0 0 0 0 7 8 1 0 3]
           [0 0 0 6 0 0 5 9 0]])))

  (is (= '[[8 2 5 7 6 1 9 3 4] 
           [1 3 4 2 5 9 7 8 6] 
           [9 7 6 8 3 4 2 1 5] 
           [4 5 9 1 8 6 3 2 7] 
           [6 1 3 4 2 7 8 5 9] 
           [2 8 7 3 9 5 4 6 1] 
           [3 9 1 5 #{4 7} 2 6 #{4 7} 8] 
           [5 6 2 9 #{4 7} 8 1 #{4 7} 3] 
           [7 4 8 6 1 3 5 9 2]]
         (solve 
         '[[0 2 5 0 0 1 0 0 0]
           [1 0 4 2 5 0 0 0 0]
           [0 0 6 0 0 4 2 1 0]
           [0 5 0 0 0 0 3 2 0]
           [6 0 0 0 2 0 0 0 9]
           [0 8 7 0 0 0 0 6 0]
           [0 9 1 5 0 0 6 0 0]
           [0 0 0 0 0 8 1 0 3]
           [0 0 0 6 0 0 5 9 0]])))
)

(run-tests)