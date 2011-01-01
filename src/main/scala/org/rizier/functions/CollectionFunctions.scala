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
   
   /** Finds the first missing number in a list of sequence (0 .. max)
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
}
 
