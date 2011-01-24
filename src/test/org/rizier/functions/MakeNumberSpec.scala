package org.rizier.functions

/**
 * Rizier Project code.
 * 
 * All the codes written here are freely distributed under 
 * Apache License. It is delivered as is and no warranty whatsoever
 * applied to the code.
 *
 * User: rizal
 * Date: 24/01/11
 * Time: 18:41
 */

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class  MakeNumberSpec extends FlatSpec with ShouldMatchers {

  "Make Number" should "create all expressions that make a number" in {

    import org.rizier.functions.MakeNumber._

    val xs = (1 to 9).toList
    solutions(xs, 100).map(displayE(_)) should equal(
      List("1 * 2 * 3 + 4 + 5 + 6 + 7 + 8 * 9",
           "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 * 9",
           "1 * 2 * 3 * 4 + 5 + 6 + 7 * 8 + 9",
           "12 + 3 * 4 + 5 + 6 + 7 * 8 + 9",
           "1 + 2 * 3 + 4 + 5 + 67 + 8 + 9",
           "1 * 2 + 34 + 5 + 6 * 7 + 8 + 9",
           "12 + 34 + 5 * 6 + 7 + 8 + 9"))

  }

}