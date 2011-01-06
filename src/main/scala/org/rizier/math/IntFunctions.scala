package org.rizier.math;

/** Utility for integer number.*/
object IntFunctions {

  /** Reverse a non negative number */
  def reverse(x: Long): Long = {
      require(x >= 0)
      def reverse(x: Long, acc: Long): Long = {
        if (x == 0) acc 
        else reverse(x / 10, (x % 10) + acc * 10)
     }
     reverse(x, 0)
  }
  
  /** Checks if a number is a palindrome number.
  *   A palindrome number reads the same both ways.
  */
  def isPalindrome(x: Long) = x == reverse(x)
  
  /** Returns the list of digit of a number. */
  def digits(x: Long): List[Long] = {
    require(x >= 0)
    def digits(x: Long, acc: List[Long]): List[Long] = {
      if (x == 0) acc
      else digits( x / 10, (x % 10) :: acc)
    }
    digits(x, Nil)
 }
 
 /** Given a function f that takes a pair of natural numbers and a natural
 *   number of z, returns a lit of all pairs (x,y) satisfying f(x,y)= z.
 *   The function f is strictly increasing on each argument.
 *   The problem is known as saddleback search problem and the solution
 *   is explained in (Bird, 2010).
 */
 def invertSimple(f: (Int, Int) => Int, z: Int) = {
     import scala.annotation.tailrec
     @tailrec def findInvert(u: Int, v: Int, acc: List[(Int, Int)]):  List[ (Int, Int) ] = {
           if ( u > z || v < 0) acc
           else {
              val zz = f(u,v)
              if (zz < z) findInvert(u + 1, v, acc)
              else if (zz == z) findInvert(u + 1, v - 1, (u,v) :: acc)
              else findInvert(u, v - 1, acc)
           }
     }
     findInvert(0, z, Nil)
 }
 
 def binarySearch( f:(Int) => Int, left: Int, right: Int, z: Int): Int = {
    val mid = (left + right) / 2
    val midval = f(mid)
    if ( left > right) left
    else if (midval <= z) binarySearch(f, mid + 1, right, z)
    else binarySearch(f, 0, mid - 1, z)
    
     
 }
 
 /** The optimized version of invert simple where the search starts
 *   and ends not at (0,z) to (z, 0), but from (0,m) to (n, 0). The
 *   optimization is done by deploying binary search to discover 
 *   m and n.
 */
 def invert(f:(Int, Int) => Int, z: Int) = {
     
     def findInvert( u: Int, v:Int, r:Int, s: Int, acc: List[ (Int, Int)]):
        List[ (Int, Int) ]= {
        if (u > r || v < s) acc
        else {

            val zz = f(u,v);
            if (zz < z) findInvert(u + 1, v, r, s, acc)
            else if (zz == z) findInvert(u + 1, v - 1, r, s, (u,v) :: acc)
            else findInvert(u, v - 1, r, s, acc)
        }
     }
     findInvert(0,
        binarySearch(f(0, _), 0, z, z), 
        binarySearch(f(_,0), 0, z, z), 
        0, 
        Nil)  
 }
}




