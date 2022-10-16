package test
import org.scalatest.funsuite.AnyFunSuite
import uebung1.Rational

class RationalTest extends AnyFunSuite {

  test("Rational Inititalisation 1") {
    val x = new Rational(1,2)
    assert(x.value === 0.5)
  }
  
  test("Rational Inititalisation 2") {
    val x = new Rational(1,2)
    assertResult("1/2"){x.toString}
  }

  test("Test requirement (denominator!=0)"){
      intercept [IllegalArgumentException] {
        new Rational(1,0)}
  }

  test("Add 0 to Rational") {
    val x = new Rational(3, 5)
    val zero = new Rational (0, 1)
    val result = x.add(zero)
    assert(result.value === x.value)
  }

  test("Add Rational to Rational") {
    val x = new Rational(1, 3)
    val y = new Rational(34, 87)
    val result = x.add(y)
    assert(result.value == (x.value + y.value))
  }

  test("Sub 0 from Rational") {
    val x = new Rational(3, 5)
    val zero = new Rational (0, 1)
    val result = x.sub(zero)
    assert(result.value === x.value)
  }

  test("Sub Rational from Rational") {
    val x = new Rational(1, 2)
    val y = new Rational(1, 8)
    val result = x.sub(y)
    assert(result.value == (x.value - y.value))
  }

  test("Negate Zero") {
    val zero = new Rational(0, 1)
    val negated = zero.neg
    assert(zero.value == negated.value)
  }

  test("Negate Rational") {
    val rational = new Rational(34, 69)
    val negated = rational.neg
    assert(-rational.value == negated.value)
  }
}
