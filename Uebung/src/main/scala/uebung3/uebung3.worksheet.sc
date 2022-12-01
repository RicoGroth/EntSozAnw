import scala.annotation.tailrec

def sqrt(x: BigDecimal): BigDecimal = {
    def sq(estimation: BigDecimal): BigDecimal = {
        val epsilon = 0.0000000001
        if((estimation * estimation - x).abs < epsilon) 
            estimation
        else 
            sq((x / estimation + estimation) / 2)
    }
    sq(x/2)
}

def nthPrime(n: Int): BigInt = {
    def isPrime(x: BigInt): Boolean = {
        def f(current: BigInt): Boolean = {
            if(current <= 1) true
            else if(x % current == 0) false
            else f(current - 1)
        }
        f(sqrt(BigDecimal(x)).toInt)
    }

    @tailrec
    def f(counter: Int, current: BigInt): BigInt = {
        val p = isPrime(current)
        if(counter <= 1 && p) current
        else if(p) f(counter - 1, current + 1)
        else f(counter, current + 1)
    }
    if (n <= 1) 2 else f(n, 2)
}

// answer: 104743
//nthPrime(10001)

def fibonacci(n: Int): BigInt = n match{
    case 0 => 0
    case 1 => 1
    case _ => fibonacci(n - 1) + fibonacci(n - 2)
}

def fibonacciOpt(n: Int): BigInt = ???

def convert(convertTo: String, temperature: Int): Int = ???