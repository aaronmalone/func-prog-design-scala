package calculator

import calculator.TweetLength.MaxTweetLength
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, _}

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("calculator: literals") {
    val result = Calculator.eval(Literal(Math.PI), Map.empty)
    assert(result == Math.PI)
  }

  test("calculator: ref") {
    val literalE = Literal(Math.E)
    val result = Calculator.eval(Ref("foo"), Map("foo" -> Signal(literalE)))
    assert(result == Math.E)
  }

  test("calculator: undefined variable") {
    val result = Calculator.eval(Ref("foo"), Map.empty)
    assert(result.isNaN)
  }

  test("calculator: plus") {
    val e1 = Literal(3)
    val e2 = Literal(4)
    val p = Plus(e1, e2)
    val result = Calculator.eval(p, Map.empty)
    assert(result == 7)
  }

  test("calculator: times") {
    val e1 = Literal(2)
    val e2 = Literal(5)
    val t = Times(e1, e2)
    val result = Calculator.eval(t, Map.empty)
    assert(result == 10)
  }

  test("calculator: minus") {
    val n1 = 8
    val n2 = 5
    val m = Minus(Literal(n1), Literal(n2))
    val result = Calculator.eval(m, Map.empty)
    assert(result == 3)
  }

  test("calculator: divide") {
    val n1 = 64
    val n2 = 16
    val d = Divide(Literal(n1), Literal(n2))
    val result = Calculator.eval(d, Map.empty)
    assert(result == 4)
  }

  test("calculator: cyclic dependencies") {
    val a = Plus(Ref("b"), Literal(1))
    val b = Times(Literal(2), Ref("a"))
    val refs = Map("a" -> a, "b" -> b)
    val result = Calculator.eval(Ref("a"), refs mapValues {Signal(_)})
    assert(result.isNaN)
  }

}
