package uebung1

class Rational (val numerator: Int, val denominator: Int){

  def this (denom: Int) = this(1,denom)  
  override def toString: String = s"$num/$denom"
  val gcd: Int = {
    val result = math.abs(Rational.gcd(numerator, denominator))
    if ((numerator > 0 && denominator < 0) || (numerator < 0 && denominator < 0))
      -result
    else
      result
  }
  require (denominator!=0,"Denominator muss != 0 sein")

  // TODO: check doesn't work as intended
  def num: Int=numerator/gcd
  def denom: Int= denominator/gcd
  def value: Double = (num.toDouble / denom)

  def  max(x: Rational): Rational = {
    if (numerator/denominator<x.num/x.denom) this else x
  }

  def add(x: Rational): Rational = {
    new Rational(this.num * x.denom + x.num * this.denom, this.denom * x.denom)
  }

  def neg: Rational = {
    new Rational(-this.num, this.denom)
  }

  def sub(x: Rational): Rational = {
    this.add(x.neg)
  }

  // private def gcd(a: Int, b: Int): Int = {
  //   if(b == 0) a
  //   else gcd(b, a % b)
  // }
}

/* Companion object, gcd() now works like a static method in Java 
 * i.e. gcd() can be called like this: Rational.gcd(a, b) 
 */ 
object Rational {
  def gcd(a: Int, b: Int): Int = {
    if(b == 0) a
    else gcd(b, a % b)
  }
}

