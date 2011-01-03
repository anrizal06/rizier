(ns 
  #^{:author "Anwar Rizal"
     :doc "Functions for integer.
     "}
     org.rizier.math.int-functions)

(defn reverse-num
   "Gets the reverse of a non negative number"
   [x]
   (letfn [
       (rev [x acc]
         (if (zero? x) 
              acc 
              (rev (quot x 10) (+ (rem x 10) (* acc 10)))))]
     (rev x 0)))
     
(defn is-palindrome 
   "Checks if a number is a palindrome. A palindrome number reads
   the same both ways"
   [x] 
   (= x (reverse x)))

(defn digits
   "Get the list of the digits of a non negative number"
   [x]
   (letfn [
       (digits-of [x acc]
           (if (zero? x) 
                acc 
               (digits-of (quot x 10) (cons (rem x 10) acc))))]
        (digits-of x [])))
        
; The design of the invert function that given a function f that takes
; a pair of natural numbers and a natural number z, returns a list of 
; all pairs (x,y) satisfying f(x,y)= z. The function f is strictly
; increasing on each argument.
; The problem is known as saddleback search problem.
; The solution here is explained in (Bird, 2010)
(defn invert-simple 
     "The solution to saddleback search problem, simple solution."
     [f z]
     (letfn [
     	(find-invert [u v f z]
     	   (let [zz (f u v)]
     	      (cond (or (> u z) (< v 0)) nil
     	   	   (< zz z) (find-invert (inc u) v f z)
     	   	   (= zz z) (cons [u v] (find-invert (inc u) (dec v) f z))
     	   	   	   (> zz z) (find-invert u (dec v) f z))))]
     (find-invert 0 z f z)))
     
     
(defn bsearch [g left right z]
	(let  [mid (quot (+ left right) 2)
	       midval (g mid)]
        (if (> left right) left
        	(cond  (<= midval z) (bsearch g (inc mid) right z)
                (> midval z) (bsearch g left (dec mid) z)))))
		
(defn invert [f z]
    (let [find-invert
    	  (fn find-invert [ [u v] [r s] f z]
     	     (let [zz (f u v)]
     	      (cond (or (> u r) (< v s)) nil
     	   	   (< zz z) (find-invert [(inc u) v] [r s] f z)
     	   	   (= zz z) (cons [u v] (find-invert [(inc u) (dec v)] [r s] f z))
     	           (> zz z) (find-invert [u (dec v)] [r s] f z))))
     	    m (bsearch #(f 0 %1) 0 z z)
     	    n (bsearch #(f %1 0) 0 z z)]
     	(find-invert [0 m] [n 0] f z)
     	           
 
