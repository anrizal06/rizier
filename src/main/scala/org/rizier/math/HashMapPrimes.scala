package org.rizier.math
/** 
*   The implementation of sieve algorithms using HashMap that keeps
*   the next composite number associated to a prime number.
*   The algorithm starts with the last found prime and then 
*   put in a hash map the next composite corresponding to the number. If
*   the hash map contains already the number, the search continues to find
*   the next composite number.
*/
object HashMapPrimes  {
	// for the purpose of the algorithm implementation, the hash map
	// to be used is the mutable one since we target 
	// to be able to support > 5 million primes
  import scala.collection.mutable.Map
	
    /** Creates the stream of primes.
    *   It starts with 2 , followed by 3. The map is initialized to 
    *   9 which corresponds to 3 * 3 and 6 is the incremental factor 
    *   for 3. With these two values, the algorithm will find 9 
    *   followed by 15, 21, so on as composite numbers.
    */
	def primes() : Stream[Int] = 2 #:: sieve(3, Map{ 9 -> 6 })

	private def sieve(p: Int, pQ: Map[Int, Int]): Stream[Int] =
	  p #:: sieve(nextPrime(p + 2, pQ), pQ )

	
	/** Puts the next composite number corresponding to a prime.
	*   First it checks if the number is already in the list, and if 
	*   it is the case, it recursively calls the function to find the
	*   next composite number.
	*   If the number is not in the list, it stores the number 
	*   and the corresponding step. 
	*/
	private def nextComp(
	      x:Int, step: Int, pQ: Map[Int, Int]): Unit = {
	  pQ.get(x) match {
	    case Some(_) => nextComp(x + step, step, pQ)
	    case None => pQ(x) = step
	  } 
	}
	
	/** Finds next prime from a candidate and a hash map
	*   of composite number.
	*   @param candidate prime number candidate
	*   @param pQ the hash map of the composite number.
	*/
	private def nextPrime(candidate: Int, pQ: Map[Int, Int]): Int = {
	  pQ.get(candidate) match {
	      case Some(step) =>
	          // remove the candidate from the composite number
	          // hash map, and replace it with the next composite
	          // number.
		      pQ -= candidate
		      nextComp(candidate + step, step, pQ)
		      nextPrime(candidate + 2, pQ)
	      case None =>
	          // found a prime number
	          // put the next composite number corresponding 
	          // to the prime number, which is candidate * candidate
	          // and the step = candidate * 2
	          pQ(candidate * candidate) = candidate * 2
	          candidate 
	  }
	}
}
