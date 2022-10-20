/* UEBUNG 2*/

// Aufgabe 1
def or(x: Boolean, y: => Boolean): Boolean = {
    if(x) true
    else try y catch {
        case e: Exception => false
    }
}
or(true, true)
or(true, false)
or(false, true)
or(false, false)
or(false, 5 > 1/0)

// Aufgabe 2
def myMethod(x: math.BigInt) = {
    if(x < 0) "kleiner null"
    else if(x > 0) "groesser null"
    else "null"
}

// Aufgabe 3
val x = {
    val offset = 1
    {
        val x = 2
        val offset = 10
        x + offset
    }
    +
    {
        val x = 5
        x + offset
    }
}

// Aufgabe 4
def squareUnder(x: Double, max: Double): Double = if(x * x < max) squareUnder(x * x, max) else x
squareUnder(1.020911, 200)

// Aufgabe 5
def divisor(x: Int): Int = ???
//divisor(33)

// Aufgabe 6
def fibonacci(x: Int): Int = if(x <= 1) 0 else if(x == 2) 1 else fibonacci(x - 1) + fibonacci(x - 2)
def fib(x1: Int = 0, x2: Int = 1, x: Int): Int = if(x != 3) fib(x2, x1 + x2, x - 1) else x1 + x2
fib(x = 100)

// Aufgabe 7
//myMethod(fibonacci(100))