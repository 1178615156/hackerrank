package functionalProgramming.parsers

/**
  * Created by yuJieShui on 2016/7/17.
  */
object Expressions {

  trait Factor

  case class NumberFactor(int: Int) extends Factor

  case class UnaryOperatorFactor(int: Int) extends Factor

  case class BracketsFactor(int: Int) extends Factor

  trait Term

  case class SingleTerm(factor: Factor) extends Term

  case class MultiplicationTerm(factor: Factor, term: Term) extends Term

  case class DivisionTerm(factor: Factor, term: Term) extends Term

  trait Expression

  case class SingleExpression(term: Term) extends Expression

  case class AddExpression(term: Term, expression: Expression) extends Expression

  case class SubtractionExpression(term: Term, expression: Expression) extends Expression


  def isNumber(s: Char) = 1 to 9 map (_.toString.head) contains s
  def takeNumber(s: String): (Int, String) = {
    def impl(s: String): String =
      if (isNumber(s.head)) s.head.toString + takeNumber(s.tail) else ""

    impl(s).toInt -> impl(s.drop(s.length))
  }

  def parser(s: String): Expression = {
//    s.head match {
//      case e if isNumber(e)=> val (number , other)  = takeNumber(s)
//        val singleExpression = SingleTerm(NumberFactor(number))
//        other.head match {
//          case '+' => AddExpression(singleExpression,parser(other.tail))
//          case '-' => SubtractionExpression(singleExpression,parser(other.tail))
//          case '*' => MultiplicationTerm(NumberFactor(number),parser(other.tail))
//          case '/' =>
//        }
//    }


    ???
  }
}
