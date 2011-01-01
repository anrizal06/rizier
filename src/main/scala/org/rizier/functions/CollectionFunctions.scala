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
      xs: IndexedSeq[(Int, A)] ): IndexedSeq[A] = {
        xs.view.filter(_._1 < size).foldLeft(Vector.fill(size)(initial))(
                  (v, t) => v.updated (t._1, f(v(t._1), t._2))) 
   }
}
