import scala.collection.immutable.List;

def avgEvenOdd(list: List[Int]): (Int, Int) = {
    val temp = list.foldLeft((0, 0, 0, 0))((carry, item) => {
        if(item % 2 == 0)
            (carry._1 + item, carry._2 + 1, carry._3, carry._4) 
        else
            (carry._1, carry._2, carry._3 + item, carry._4 + 1)
    })
    (temp._1 / temp._2, temp._3 / temp._4)
}

avgEvenOdd(List(2, 2, 3, 3))

def duplicate(list: List[Int]): List[Int] = list.flatMap(x => List(x, x))
duplicate(List(1, 2, 3, 4))

def kartesisch[T, U](list1: List[T], list2: List[U]): List[(T, U)] = list1.flatMap(t => list2.map(u => (t, u)))
kartesisch(List(1, 2), List('a', 'b'))

def moduloMap(list: List[Int], mod: Int): Map[Int, List[Int]] = list.foldLeft(Map.empty[Int, List[Int]])((currentMap, item) => {
    currentMap.updated(item % mod, item :: currentMap.getOrElse(item % mod, List.empty[Int]))
})

moduloMap(List(1, 2, 3, 4, 5, 6, 7), 3)


/*
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
*/
def problem1(n: Int): Int = {
    def f(carry: Int, current: Int): Int = {
        if(current < 3) carry 
        else if(current % 3 == 0 || current % 5 == 0) f(carry + current, current - 1) 
        else f(carry, current - 1)
    }
    f(0, n - 1)
}
problem1(10)
problem1(1000)

/*
By considering the terms in the Fibonacci sequence whose values do not exceed 4'000'000, find the sum of the even-valued terms.
*/
def problem2(n: BigInt): BigInt = {
    def f(fib1: BigInt, fib2: BigInt, carry: BigInt): BigInt = {
        if(fib2 > n) carry
        else if(fib2 % 2 == 0) f(fib2, fib1 + fib2, carry + fib2)
        else f(fib2, fib1 + fib2, carry)
    }
    f(1, 2, 0)
}
problem2(4000000)

/*
What is the largest prime factor of the number 600851475143 ?
*/
def problem3(n: BigInt): BigInt = {
    def sqrt(number : BigInt) = {
        def next(n : BigInt, i : BigInt) : BigInt = (n + i/n) >> 1
        val one = BigInt(1)
        var n = one
        var n1 = next(n, number)
        while ((n1 - n).abs > one) {
            n = n1
            n1 = next(n, number)
        }
        while (n1 * n1 > number) {
            n1 -= one
        }
        n1
    }

    def buildPrimes(upperBound: BigInt, currentList: List[BigInt] = List(2), begin: BigInt = 3): List[BigInt] = {
        if(begin >= upperBound) currentList
        else if(currentList.forall(p => begin % p != 0)) buildPrimes(upperBound, begin::currentList, begin + 2)
        else buildPrimes(upperBound, currentList, begin + 1)
    }
    0
}
// problem3(600851475143L)

// Generators
val list1 = List(1, 2)
val list2 = List('a', 'b')
for(i1 <- list1; i2 <- list2) yield (i1, i2)


val db =List(("francesco", "bloodsports"), ("simon", "jamesBond"), ("marcus","jamesBond"), ("francesco", "die12KammernDerShaolin"))
// people who have seen exactly two movies
for(i1 <- db; i2 <- db; if(i1._1 == i2._1 && i1._2 != i2._2)) yield i1._1
// a list of people and the films they have seen
for(a <- db) yield (a._1, for(b <- db; if(b._1 == a._1)) yield b._2)
db.map(x => (x._1, db.filter(y => x._1 == y._1).map(_._2)))


