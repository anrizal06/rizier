package org.rizier.math

/** The implementation of sieve algorithms using naive approach.
*   The only optimization done in this program is by discarding
*   all even numbers.
*/
object Primes1 {
  /** Obtains a stream of prime number. */
  def primes(): Stream[Int] = {
     lazy val ps = 2 #:: sieve(3)
     def sieve(p: Int): Stream[Int] = {
         p #:: sieve(
            Stream.from(p + 2, 2).
                find(i=> ps.takeWhile(j => j * j <= i).
                forall(i % _ > 0)).get)
     }
     ps
  } 
}
