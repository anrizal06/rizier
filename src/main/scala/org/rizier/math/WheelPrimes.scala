package org.rizier.math

/** The implementation of sieve algorithms that move forward from 
*   the naive approach by discarding the numbers that are divisible 
*   by 2, 3, 5, 7. 
*   To discard those numbers, a wheel function is used to generate
*   the next step of the number to be examined. 
*   The wheel to be used is
*   2 , 4 , 2 , 4 , 6 , 2 , 6 , 4 , 2 , 
*   4 , 6 , 6 , 2 , 6 , 4 , 2, 6 , 4 , 6 , 
*   8 , 4 , 2 , 4 , 2 , 4 , 8 , 6 , 4 , 6 , 
*   2 , 4 , 6, 2 , 6 , 6 , 4 , 2 , 4 , 6 , 
*   2 , 6 , 4 , 2 , 4 , 2 , 10 , 2 , 10
*/
object Primes2 {
   /** Creates a circular list from a list.
   *   @param xs the list to be transformed to a circular list.
   */
   def cycle(xs: List[Int]): Stream[Int] = {
     lazy val result: Stream[Int] = xs.toStream.append(result)
     result
   }
   
   /** Creates the wheel for 2, 3, 5, 7
   */
   def wheel(): Stream[Int] = {
     cycle(List(
       2 , 4 , 2 , 4 , 6 , 2 , 6 , 4 , 2 , 
       4 , 6 , 6 , 2 , 6 , 4 , 2, 6 , 4 , 6 , 
	   8 , 4 , 2 , 4 , 2 , 4 , 8 , 6 , 4 , 6 , 
	   2 , 4 , 6, 2 , 6 , 6 , 4 , 2 , 4 , 6 , 
	   2 , 6 , 4 , 2 , 4 , 2 , 10 , 2 , 10))
   }
   
   /** Creates an infinite stream of long from an iterator.
   *   The function can be seen as the general form of Stream.from 
   *   that takes a fix number of step. The from here accepts an iterator
   *   of long instead. Using the iterator, it is possible then
   *   to use the wheel as the incrementing factor.
   *
   *   @param x the starting point
   *   @stepIt the iterator.
   */
   def from(x: Int, stepIt: Iterator[Int]): Stream[Int] = {
       x #:: from(x + stepIt.next, stepIt)
   }
   
   /** The algorithms start from the last known prime. A new prime
   *   is calcuated by finding a new number that is not divisable by all
   *   primes less than its square root. The new number is generated
   *   using the wheel iterator.
   *   Note the use of lazy val ps that is used to keep the number of 
   *   primes discovered so far.
   *   Example of use: ps.takeWhile(_ < 10000).toList creates 
   *   a list of prime number less than 10000
   */
   def primes(): Stream[Int] = {
       lazy val ps = 2 #:: 3 #:: 5 #:: 7 #:: sieve(11, wheel.iterator)
       def sieve(p: Int, it: Iterator[Int]): Stream[Int] = {
           p #:: sieve( from(p + it.next, it).
                           find(i => ps.takeWhile(j => j * j <= i).
                            forall(i % _ > 0)).get,
                         it)
       }
       ps
   }
}
