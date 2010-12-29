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
 
}


