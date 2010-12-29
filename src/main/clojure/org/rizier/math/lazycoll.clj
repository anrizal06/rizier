(ns 
  #^{:author "Anwar Rizal"
     :doc "A couple of lazy collection algorithms"}
  org.rizier.math.lazycoll)

(defn fibonacci 
  "Fibonacci number generator"
  []
  (letfn [ 
      (fib [a b]
      	 (let [n (+ a b)]
         (cons  n (lazy-seq (fib b n)))))] 
  (concat [0 1](lazy-seq (fib 0 1)))))

; not too interesting one, since this is exactly 
; the version of power-of-two of clojure contrib
(defn power-of-two
   "Return infinite number of power of 2"
   []
   (iterate #(bit-shift-left % 1) 1))
   
; EXPERIMENTAL, not sure to be very useful.
(defn factorial
   "Return infinite number of factorial number."
   []
   (letfn [
       (fact [a b]
       (cons b(lazy-seq (fact (inc a) (* b (inc a))))))]
    (fact 1 1)))
