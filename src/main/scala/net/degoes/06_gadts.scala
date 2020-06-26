package net.degoes
import net.degoes.expr.CalculatedValue.Str
import net.degoes.expr.CalculatedValue.Bool
import net.degoes.expr.CalculatedValue.Add
import net.degoes.expr.CalculatedValue.Subtract
import net.degoes.expr.CalculatedValue.Multiply
import net.degoes.expr.CalculatedValue.Concat
import net.degoes.expr.CalculatedValue.StartsWith
import zio.UIO

/*
 * INTRODUCTION
 *
 * In Functional Design, type safety of data models can be greatly improved by
 * using so-called generalized algebraic data types.
 *
 * In this section, you'll review GADTs with a focus on functional models.
 *
 */

object Motivation {
  //Pipeline is an immutable datatype that models a tf from an input data set into an output data set as a series of individual operations
  // Pipeline[A]

  // Turtle
  // Turtle[A] models a data type that models a series of drawing operations on a canvas

  //Expr[A] is a model of computing an integer or string expression to yield a value of type A



  // What is A ? I say that when evaluated, that little program will yield an A (this is done in the `eval` method)

  /**
    * Look at the Eval. it is polymorphic in A, so it should work with any A. Now, it is not a contradicion because we are extending a 
    * Expr[A] with a finite combination of possibilities. It allows us to return an Int even if we are expecting an Expr[A]. But this 
    * can happen only when I feed there a ConstantInt <: Expr[Int]. so value: Expr[Int] and value: Expr[A] => A === Int
    * So we had to return an A, we return an Int because we know in that case A *is* an Int!
    * This kind of deduction is done by the Scala compiler
    * 
    * N.B.: Polymorphic ADT with the A is not interesting, what is interesting is the SPECIALIZATION!!!
    */

  sealed trait Expr[A] { self => 
    def + (that: Expr[A])(implicit aIsInt: A <:< Int): Expr[Int] = 
      Expr.Add(self.as[Int], that.as[Int])

    def as[B](implicit ev: A <:< B): Expr[B] = Expr.As(self, ev)
  }

  //alternative:
  sealed trait Expr2[A] { self => 
  }
  object Expr2 {
    //extension method
    implicit class ExprInt(self: Expr2[Int]) {
      def + (that: Expr2[Int]): Expr2[Int] = Expr2.Add
    }

    final case class Add(left: Expr2[Int], right: Expr2[Int]) extends Expr2[Int]

  }
  object Expr {
    final case class ConstantInt(value: Int) extends Expr[Int]
    final case class ConstantStr(value: String) extends Expr[String]
    final case class As[A, B](expr: Expr[A], ev: A <:< B) extends Expr[B]
    final case class Add(left: Expr[Int], right: Expr[Int]) extends Expr[Int]

    def int(v: Int): Expr[Int] = ConstantInt(v)
    def str(v: String): Expr[String] = ConstantStr(v)
  }

  def eval[A](expr: Expr[A]): A = 
    expr match {
      case Expr.Add(left, right) => eval(left) + eval(right)

      case Expr.ConstantInt(v) => v

      case Expr.ConstantStr(v) => v

      case Expr.As(expr, ev) => ev(eval(expr))
    }
  // eval(Expr.str("foo") + Expr.str("bar"))
}

/**
 * EXPRESSIONS - EXERCISE SET 1
 *
 * Consider an application (such as the spreadsheet example) that needs to
 * calculate values in a user-defined way.
 * 
 */
object expr {
  sealed trait CalculatedValue[+A]
  object CalculatedValue {
    final case class Integer(value: Int) extends CalculatedValue[Int]
    final case class Str(value: String)  extends CalculatedValue[String]
    final case class Bool(value: Boolean) extends CalculatedValue[Boolean]

    /**
     * EXERCISE 1
     *
     * Add an operator that adds two integer expressions, yielding an integer
     * expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     * 
     * Why not Add(first: Integer, second: Integer)
     */
    final case class Add(first: CalculatedValue[Int], second: CalculatedValue[Int]) extends CalculatedValue[Int]

    /**
     * EXERCISE 2
     *
     * Add an operator that subtracts an integer from another integer expression,
     * yielding an integer expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Subtract(first: CalculatedValue[Int], second: CalculatedValue[Int]) extends CalculatedValue[Int]

    /**
     * EXERCISE 3
     *
     * Add an operator that multiplies two integer expressions, yielding an
     * integer expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Multiply(first: CalculatedValue[Int], second: CalculatedValue[Int]) extends CalculatedValue[Int]

    /**
     * EXERCISE 4
     *
     * Add an operator that concatenates two strings, yielding a string
     * expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Concat(first: CalculatedValue[String], second: CalculatedValue[String]) extends CalculatedValue[String]

    /**
     * EXERCISE 5
     *
     * Add an operator that determines if a string starts with a specified
     * prefix, yielding a boolean expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class StartsWith(prefix: CalculatedValue[String], value: CalculatedValue[String]) extends CalculatedValue[Boolean]
  }

  import CalculatedValue._


  // a generic interpreter is 
  type Interpreter[F[_], A, G[_], B] = F[A] => G[B]

  type PrettyPrinter[A] = Interpreter[CalculatedValue, A, UIO, String]
  def calculate[A](expr: CalculatedValue[A]): A =
    expr match {
      case Integer(value) => value
      case Str(value) => value
      case Bool(value) => value
      case Add(first, second) => calculate(first) + calculate(second)
      case Subtract(first, second) => calculate(first) - calculate(second)
      case Multiply(first, second) => calculate(first) * calculate(second)
      case Concat(first, second) => calculate(first) + calculate(second)
      case StartsWith(prefix, value) => calculate(value).startsWith(calculate(prefix))
    }

  def calculateZIO[A](expr: CalculatedValue[A]): UIO[A] =
    expr match {
      case Integer(value) => UIO(value)
      case Str(value) => UIO(value)
      case Add(left, right) => (calculate(left) zipWith calculate(right))(_ + _)
      case Subtract(left, right) => (calculate(left) zipWith calculate(right))(_ - _)
      case Multiply(left, right) => (calculate(left) zipWith calculate(right))(_ * _)
      case Concat(left, right) => (calculate(left) zipWith calculate(right))(_ + _)
      case StartsWith(left, right) => (calculate(left) zipWith calculate(right))(_ startsWith _)
    }     
}

/**
 * PARSERS - EXERCISE SET 2
 */
object parser {
  sealed trait Parser[+A]
  object Parser {
    final case object OneChar extends Parser[Char]

    /**
     * EXERCISE 1
     *
     * Add an operator that can repeat a parser between some lower range
     * (optional) and some upper range (optional).
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Repeat[A](min: Option[Int], max: Option[Int], parser: () => Parser[A]) extends Parser[List[A]]

    /**
     * EXERCISE 2
     *
     * Add a constructor that models the production of the specified value (of
     * any type at all), without consuming any input.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Succeed[A](value: A) extends Parser[A]

    /**
     * EXERCISE 3
     *
     * Add a constructor that models failure with a string error message.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Fail(value: String) extends Parser[Nothing]

    /**
     * EXERCISE 4
     *
     * Add an operator that can try one parser, but if that fails, try
     * another parser.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class OrElse[A, B](left: () => Parser[A], right: () => Parser[B]) extends Parser[Either[A, B]]

    /**
     * EXERCISE 5
     *
     * Add an operator that parses one thing, and then parses another one,
     * in sequence, producing a tuple of their results.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Sequence[A, B](left: () => Parser[A], right: () => Parser[B]) extends Parser[(A, B)]
  }

  import Parser._

  def parse[A](parser: Parser[A], input: String): Either[String, (String, A)] =
    parser match {
      case OneChar =>
        input.headOption
          .map((a: Char) => Right(input.drop(1) -> a))
          .getOrElse(Left("The input to the parser has no remaining characters"))

      case Repeat(min0, max0, parser) => 
        val min = min0.getOrElse(0)
        val max = max0.getOrElse(Int.MaxValue) 

        parse(parser, input) match {
          case Left(error) => if (min > 0) Left(error) else Right((input, Nil))
          case Right((input, value)) => 
            parse(Repeat(Some(min - 1), Some(max), parser), input) match {
              case Left(error) => if (min > 1) Left(error) else Right((input, value :: Nil))
              case Right((input, tail)) => Right((input, value :: tail))
            }
        }

      case Succeed(value) => Right((input, value))

      case Fail(value) => Left(value)

      case OrElse(left, right) =>
        parse(left(), input).map { case (input, a) => (input, Left(a)) } match {
          case Left(error) => 
            parse(right(), input).map { case (input, b) => (input, Right(b)) }
          case either => either
        }

      case Sequence(left, right) =>
        parse(left(), input) match {
          case Left(error) => Left(error) 
          case Right((input, a)) => 
            parse(right(), input).map { 
              case (input, b) => (input, (a, b))
            }
        }
    }
}