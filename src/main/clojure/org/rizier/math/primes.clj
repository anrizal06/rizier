(ns 
  #^{:author "Anwar Rizal"
     :doc "The prime generator algorithms implementation."}
  org.rizier.math.primes)
  
; The implementation of primes using lazy but naive 
; sieve algorithm.
; The algorithm lazily creates a new prime from the list 
; of all known primes.
(defn naive-primes
   "Implementation of lazy but naive sieve algorithms"
   []
  (letfn [
          (next-prime [x xs] 
             (if (some #(zero? (rem x %))
                    (take-while #(<= (* % %) x) xs))
             (recur (+ x 2) xs)
             (cons x (lazy-seq (next-prime (+ x 2) (conj xs x))))))]
   (cons 2 (lazy-seq (next-prime 3 [])))))

; The implementation of primes generation by using wheel 
; of number used to discard composite numbers that are factor
; of first prime numbers.
; The wheel can be found from (ONeill 2009).
(defn wheel-primes 
   "The optimization of lazy naive sieve algorithm 
   by discarding the number divisible by 2, 3, 5, and 7.
   This is done by adding the next number to be examined
   by the number given by the wheel."
   []
   (let [next-prime
   	 (fn next-prime [x xs [f & r]] 
             (if (some #(zero? (rem x %))
                    (take-while #(<= (* % %) x) xs))
             (recur (+ x f) xs r)
             (cons x (lazy-seq (next-prime (+ x f) (conj xs x) r)))))
         wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
			6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                        2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
         (concat 
             [2 3 5 7]
             (lazy-seq (next-prime 11 [] wheel)))
   )
)

; The sieve algorithm that creates lazy sieve algorithms by
; storing in a hashtable the next composite number corresponding 
; to a prime number.
; See  http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
; for the detail of the algorithm.
; The hashtable contains the next composite number as the key 
; and the step as the value. The step is used to find the next 
; composite number not in the hashmap.
; Whenever a new prime is found, the new entry corresponding to 
; the square of the prime is pushed to the hashtable. The step
; corresponds to the double of the prime number.
; For example, when 13 is examined, 
; the hashtable contains already (15, 6), (25, 10), (49, 14), (121, 11).
; Since 13, is not already in the map, 13 is added to the hashtable:
;  (15, 6), (25, 10), (49, 14), (121, 11), (169, 13).
; Then when 15 is examined, the program detects that 15 is already in the list,
; and then consider 15 as the composite number. Furthermore, the program
; looks for the next composite number, which is 15 + 6 = 21. 
; After 15 examination, the hashtable becomes
; (21, 6), (25, 10), (49, 14), (121, 11), (169, 13).
; 
(defn hashtable-primes 
   "An implementation of lazy sieve algorithm by storing in a hastable
   the next composite number corresponding to a prime number"
   []
  (letfn [
    (next-composite [x step sieve]
       (if (sieve x)
       	  ; the composite number is already found previously
	  (recur (+ x step) step sieve)
	  ; new composite number
	  (assoc sieve x step)))
    (next-prime [x sieve] 
      (let [step (sieve x)]	    
       (if (sieve x)
          ; found a composite
          (recur (+ x 2) (next-composite (+ x step) step (dissoc sieve x)))
          ; found a prime
	  (cons x 
	    (lazy-seq 
               (next-prime (+ x 2) (assoc sieve (* x x) (* x 2))))))))]
  (cons 2 (lazy-seq (next-prime 3 {})))))
