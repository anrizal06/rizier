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
