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
 * Time: 17:47
 */

/**The implementation of make century pearl from (Bird, 2010) book (Pearl no.6).
 *
 */
object MakeNumber {
  /** A factor is a list of digit. */
  type Factor = List[Int]

  /** A term is a list of factor. */
  type Term = List[Factor]

  /** An expression is a list of term. */
  type Expr = List[Term]

  private def emptyExpression: Expr = List(List(Nil))

  /** Intermediate class, containing the power of 10 (k), the last
   *  factor (f), the last term (t), and the value of the last expression.
   */
  private case class Intermediate(k: Int, f: Int, t: Int, e: Int) {
    def value = f * t + e
  }

  /**Glues a new digit to existing expression.
   *  When the existing expression is an empty expression, then it
   *  returns 10, x, 1, 0. Otherwise, it returns the three possibilities:
   *  concatenating to the last factor, adding to the last term and multiplying
   *  to the last factor.
   *
   */
  private def glue(expr: Expr, x: Int, im: Intermediate):
      List[(Expr,Intermediate)]=  {

    expr match {
      case (Nil::Nil)::Nil =>
        val exp = List(List(List(x)))
        List( (exp, Intermediate(10, x, 1, 0)) )

      case (xs::xss):: xsss =>
        List( ( ((x::xs)::xss)::xsss,
                Intermediate(10*im.k, im.k * x + im.f , im.t, im.e)),
              ( (List(x) :: xs::xss) :: xsss,
                Intermediate(10, x, im.f * im.t, im.e)),
              ( (List(List(x)) :: (xs::xss):: xsss),
                Intermediate(10, x, 1, im.f*im.t + im.e)))
      case _ => Nil
    }
  }

  /** Gets the solutions. */
  def solutions(xs: List[Int], n: Int): List[Expr] = {

     def ok(exp: (Expr, Intermediate)) = exp._2.value  <= n
     def good(exp: (Expr, Intermediate)) = exp._2.value == n

     xs.foldRight(List( (emptyExpression, Intermediate(0,0,0,0))))(
        (x: Int, ys: List[(Expr, Intermediate)]) =>
          ys.flatMap( y =>
            glue(y._1, x, y._2).filter(ok))).
        filter(good).unzip._1
  }

  def displayF(f: Factor) = f.reduceLeft(10 * _ + _).toString
  def displayT(t: Term)   = t.map(displayF(_)).mkString(" * ")
  def displayE(e: Expr)   = e.map(displayT(_)).mkString(" + ")



}