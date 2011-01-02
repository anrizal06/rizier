(ns 
  #^{:author "Anwar Rizal"
     :doc "Utility functions for collection."}
org.rizier.functions.collfunctions
   (:use [clojure.contrib.seq-utils :only [positions]] ))

(defn accumulate 
   "Reduce a vector xs to another vector by applying f to the second 
    part of a tuple which shares the same first tuple.
    Example: (accumulate + 0 7 [ [4 1] [4 2] [5 8]])
    returns [ [0 0] [1 0] [2 0] [3 0] [4 3] [5 8] [6 0]] .
    This is similar to accumArray function of Haskell."    
   [f initial size xs]
   (letfn [
      (apply-f [f xs tuple] (f (nth xs (first tuple)) (second tuple)))]
   (reduce #(assoc %1 (first %2) (apply-f f %1 %2) )
      (vec (repeat size initial))
      (filter #(< (first %) size) xs))))
      
; implementation of the array-based solution for the smallest free number
; problem (Bird, 2010)
(defn missing-min 
	"Finds lazily the  missing numbers in a range (0 .. n).
	For example (8, 4, 3, 2, 1, 6, 5, 0, 10) returns 7
	(8, 4, 3, 2, 1, 5, 0, 10, 11, 12) returns (6, 7, 9)"
     [xs]
     (positions #(not %)
     	 (accumulate #(or %1 %2)
     	             false
     	             (count xs)
                     (zipmap xs (cycle [true])))))
                     
                     
; implementation of the divide and conquer solution for the smallest 
; free number problem (Bird, 2010)
; it can only return the smallest missing number, not the list of 
; missing number.
; The algorithm is explained in (Bird, 2010) with the following as example:
; (8, 4, 3, 2, 1, 6, 5, 0, 10) 
; b = 0 + 1 + (9 div 2) = 5, so the list is partitioned into two
; parts < 5 and > 5:
; (4, 3, 2, 1, 0) and (8, 6, 5, 10)
; We expect 5 numbers at the left side, which is the case, so 
; the missing number must be on the right hand side: (8, 6, 5, 10), then the
; function is recusively called:
; b = 5 + 1 + (4 div 2)
; new partition = (6, 5) and (8, 10)
; We expect to have 3 at the left partition, but we don't; so the missing 
; number must be on the left partition.
; recursively called the function with 5 (6, 5)
; b = 5 + 1 + (2 div 2) = 7
; partition = (6, 5) and nil. Since we expect 2 on the left hand side, 
; and indeed we have 2, it must be on the right hand side then.
; Hence, 7.
(defn missing-min-div-conquer
	"Find lazily the missing number in a range (0..n).
	For example (8, 4, 3, 2, 1, 6, 5, 0, 10) returns 7
	(8, 4, 3, 2, 1, 5, 0, 10, 11, 12) returns 6.
	In the case where nothing is missing, it returns n + 1."
	[xs]
	(letfn [(min-from [a n xs]
		(let [b (+ a 1 (quot n 2))
		      ; (us, vs) = xs partition (< b)
		      parts (group-by #(< % b) xs)
		      us (parts true)
		      vs (parts false)
		      m (count us)]
		  (if (zero? n) a
		     (if (= (- b a) m) 
		        (min-from b (- n m) vs)
		        (min-from a m us)))))]
         (min-from 0 (count xs) xs)))
           	   
           	   
           	   
           	   
