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
def divisorDown(x: Int, div: Int): Int = if(x % div == 0) div else divisorDown(x, div - 1)
def divisorDown(x: Int): Int = divisorDown(x, x / 2)
divisorDown(33)
def divisorUp(x: Int, state: Int, res: Int): Int = if(state == x / 2) res else if(x % state == 0) divisorUp(x, state + 1, state) else divisorUp(x, state + 1, res)
def divisorUp(x: Int): Int = divisorUp(x, 1, 0)
divisorUp(33)

// Aufgabe 6
def fibonacci(x: Int): Int = {
    def fib(x: Int, res: Int): Int = ???
    fib(x, 0)
}
// def fibonacci(x: Int): Int = if(x <= 1) 0 else if(x == 2) 1 else fibonacci(x - 1) + fibonacci(x - 2)
// def fib(x1: Int = 0, x2: Int = 1, x: Int): Int = if(x != 3) fib(x2, x1 + x2, x - 1) else x1 + x2
// fib(x = 100)

// Aufgabe 7
//myMethod(fibonacci(100))

def multiple3And5(x: Int): Int = {
    if(x - 1 <= 0) 0
    else if((x - 1) % 3 == 0 || (x - 1) % 5 == 0) (x - 1) + multiple3And5(x - 1)
    else multiple3And5(x - 1)
}

multiple3And5(10)

def sqrt(x: Double): Double = {
    def sq(x: Double, estimation: Double): Double = {
        val epsilon = 0.0000000001
        if(math.abs(estimation * estimation - x) < epsilon) 
            estimation
        else 
            sq(x, (x / estimation + estimation) / 2)
    }
    sq(x, x/2)
}
sqrt(2)