package org.rizier.functions

/** Couple of collection functions. */
object CollectionFunctions {
   /** Reduces an IndexedSeq (Int, A) xs to an IndexedSeq[A] 
   *   by applying f to the xs element with the same first part of the tuple.
   *   Example: accumulate( _ + _ , 0, 7, List( (4, 1), (4, 2), (5, 8)) )
   *   returns List((0,0), (1, 0), (2, 0) , (3, 0), (4, 3), (5,8), (6,0))
   *   because it adds 1 + 2 associated to 4.
   *   This function is similar to accumArray function of Haskell.
   */
   def accumulate[A](
      f: (A, A) => A, 
      initial: A, 
      size: Int, 
      xs: List[(Int, A)] ): IndexedSeq[A] = {
        xs.view.filter(_._1 < size).foldLeft(Vector.fill(size)(initial))(
                  (v, t) => v.updated (t._1, f(v(t._1), t._2))) 
   }
   
   /** Finds the first missing number in a list of sequence (0 .. n)
   *   For example (8, 4, 3, 2, 1, 6, 5, 0, 10) returns Some(7)
   *   because 7 is the minimum missing number.
   */
   def missingMin(xs: List[Int]): Option[Int] = 
       accumulate( (a: Boolean, b: Boolean) => a || b, 
                    false,
                    xs.size,
                    xs.zip(List.fill(xs.size) (true) )).findIndexOf(! _) match {
           case x if (x > 0) => Some(x)
           case _ => None
       }
       
   /** Find the first missing number in a list of sequence (0.. n)
   *   using divide and conquer algorithm (Bird 2010). In the case where nothing
   *   is missing, it returns n + 1
   *   The algorithm is explained in (Bird, 2010) with the following as example:
   * (8, 4, 3, 2, 1, 6, 5, 0, 10) 
   * b = 0 + 1 + (9 div 2) = 5, so the list is partitioned into two
   * parts < 5 and > 5:
   * (4, 3, 2, 1, 0) and (8, 6, 5, 10)
   * We expect 5 numbers at the left side, which is the case, so 
   * the missing number must be on the right hand side: (8, 6, 5, 10), then the
   * function is recusively called:
   * b = 5 + 1 + (4 div 2)
   * new partition = (6, 5) and (8, 10)
   * We expect to have 3 at the left partition, but we don't; so the missing 
   * number must be on the left partition.
   * recursively called the function with 5 (6, 5)
   * b = 5 + 1 + (2 div 2) = 7
   * partition = (6, 5) and nil. Since we expect 2 on the left hand side, 
   * and indeed we have 2, it must be on the right hand side then.
   * Hence, 7.*/
   def missingMinDivConquer(xs: List[Int]) = {
       def minFrom(a: Int, xs: List[Int]): Int = {
          val b = a + 1 + (xs.size / 2)
          val (us, vs) = xs.partition(_ < b)
          if (xs.size == 0) a
          else if ( us.size == (b -a) ) minFrom(b, vs)
          else minFrom(a,us)
       }
       minFrom(0, xs)
   }
}
 
