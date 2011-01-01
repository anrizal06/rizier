(ns 
  #^{:author "Anwar Rizal"
     :doc "Utility functions for collection."}
  org.rizier.functions.collfunctions)

(defn accumulate 
   "Reduce a vector xs to another vector by applying f to the second 
    part of a tuple which shares the same first tuple.
    Example: (accumulate + 0 7 [ [4 1] [4 2] [5 8]])
    returns [ [0 0] [1 0] [2 0] [3 0] [4 3] [5 8] [6 0]] .
    This is similar to accumArray function of Haskell."    
   [f initial size xs]
   (letfn [
      (fill-vector [size n]  (vec (for [x (range 0 size)] n)))
      (apply-f [f xs tuple] (f (nth xs (first tuple)) (second tuple)))]
   (reduce #(assoc %1 (first %2) (apply-f f %1 %2) )
      (fill-vector size initial)
      (filter #(< (first %) size) xs))))
