(ns 
  #^{:author "Anwar Rizal"
     :doc "The prime generator algorithms implementation."}
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
           
           
