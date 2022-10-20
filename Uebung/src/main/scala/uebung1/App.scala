package uebung1

object App {

  def main(args: Array[String]): Unit = {
    
    println("Hier startet das erste Scala Programm!")
    val x = new Rational(-1, -4)
    val y = new Rational(2, 4)
    // infix-notation, x.add(y) == x add y
    println(x add y)
    // + can be used as an identifier, so x.add(y) == x + y (+ has to be a defined function, but still)
    println(x + y)
    // same here (has to be defined as unary_- since neg doesn't have parameters)
    println(-x)
  }
}