package org.rizier.math;

/** The collection of algorithms for lazy collection. Inspired  
*   from lazyseq namespace from clojure contrib.
*/
object LazyCollection {

  /** Returns the stream of fibonacci number.*/
  def fibonacci(): Stream[Long] = {
      def fib(a: Long, b: Long): Stream[Long] = 
         (a + b) #:: fib(b, a + b)
      0 #:: 1 #:: fib(0, 1)
  }
  
  /** Returns the stream of power of 2. */
  def powerOf2(): Stream[Long] = Stream.iterate(2)(_ << 1)
  
  /** Returns the stream of factorial number
  *  (EXPERIMENTAL: not sure to be very useful)
  */
  def factorial(): Stream[BigDecimal] = {
     def fact(a: BigDecimal, b: BigDecimal): Stream[BigDecimal] =
        b #:: fact((a + 1), b * (a + 1))    
     fact(1, 1)
  }

}
