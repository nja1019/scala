import hw.parsing._
import scala.util.parsing.combinator._

//1. Implement ArithEval. This is a simple recursive function.
object ArithEval extends ArithEvalLike{

	def eval(e: Expr): Double = e match {
		case Num(x)=> x
		case Add(x,y) => eval(x) + eval(y)
		case Sub(x,y) => eval(x) - eval(y)
		case Mul(x,y) => eval(x)*eval(y) 
		case Div(x,y) => eval(x)/eval(y)
		case Exponent(x,y) => Math.pow(eval(x),eval(y))
	}
}

//2. Implement ArithParser by translating the grammar provided above to Scala's parser combinators.
object ArithParser extends ArithParserLike{

	// number : PackratParser[Double] is defined in ArithParserLike
	
	lazy val atom: PackratParser[Expr] = number^^{case x => Num(x)}|expr|"("~>expr<~")"

	lazy val exponent: PackratParser[Expr] = ((exponent~"^"~atom))^^{case x~"^"~y => Exponent(x,y)}|atom
	
	lazy val add: PackratParser[Expr] = ((mul~"+"~add)|(mul~"-"~add))^^{
		case x~"+"~y => Add(x,y)
		case x~"-"~y => Sub(x,y)
	}|mul

	lazy val mul: PackratParser[Expr] = ((exponent~"*"~mul)|(exponent~"/"~mul))^^{
		case x~"*"~y => Mul(x,y)
		case x~"/"~y => Div(x,y)
	}|exponent
	lazy val expr: PackratParser[Expr] = add
}

//3. Implement ArithPrinter. We suggest using ScalaCheck to test these functions.
//(You'll have to define generators as part of your test suite.)
object ArithPrinter extends ArithPrinterLike {
	def print (e: Expr): String = e match {
		case Num(x) => x.toString
		case Add(x,y) => "(" + print(x) + "+" + print(y) + ")"
		case Sub(x,y) => "(" + print(x) + "-" + print(y) + ")"
		case Mul(x,y) => "(" + print(x) + "*" + print(y) + ")"
		case Div(x,y) => "(" + print(x) + "/" + print(y) + ")"
		case Exponent(x,y) => "(" + print(x) + "^" + print(y) + ")"
	}
}
