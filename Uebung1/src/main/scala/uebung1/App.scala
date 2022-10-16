package uebung1

object App {

  def main(args: Array[String]): Unit = {
    
    println("Hier startet das erste Scala Programm!")
    val x = new Rational(1, 4)
    val y = new Rational(2, 4)
    println(x.sub(y))
  }
}