import scala.annotation.tailrec;
import scala.collection.immutable.Map;

def quersumme(x: Int): Int = if(x < 10) x else x % 10 + quersumme(x / 10)
// Was ist die kleinste positive Zahl, die durch alle Zahlen von 1-20 ohne Rest teilbar ist? 
def dividableUpToN(n: Long): Long = {
    def divide(x: Long, divisor: Long): Long = if(divisor <= 1) x else if(x % divisor != 0) divide(x * divisor, divisor - 1) else divide(x, divisor - 1)
    divide(n, n)
}

def c2520(n: Int): Int = {
    def divide(x: Int, divisor: Int): Int = {
        println("x: " + x + ", divisor: " + divisor)
        if(divisor == 1)
            x
        else if(x % divisor == 0)
            divide(x, divisor - 1)
        else divide(n + 1,n)
    }
    divide(n, n)
}

c2520(2)

trait IntList {
    def isEmpty: Boolean
    def head: Int
    def tail: IntList
    def prepend(x: Int): IntList = new Node(x, this)
    def contains(x: Int): Boolean
    def get(index: Int): Int
    def delete(x: Int): IntList
    def deleteAll(x: Int): IntList
    def prefix(other: IntList): IntList
    def flip: IntList
    def size: Int
    def avg: Int
    def maxDistance: Int
    def maxAdjacentDistance: Int
    def max: Int
    def min: Int
    def filterEven: IntList
    def map(f: Int => Int): IntList
    def filter(predicate: Int => Boolean): IntList
    def fold(start: Int, f: (Int, Int) => Int): Int
    def forAll(predicateFunc: Int => Boolean): Boolean
}

object Empty extends IntList {
    def isEmpty = true
    def head = throw new Error("head.null")
    def tail = throw new Error("tail.null")
    def contains(x: Int): Boolean = false
    override def toString = "Empty"
    def get(index: Int): Int = head
    def delete(x: Int): IntList = Empty
    def deleteAll(x: Int): IntList = Empty
    def prefix(other: IntList): IntList = if(other.isEmpty) this else new Node(other.head, this.prefix(other.tail))
    def flip: IntList = Empty
    def size: Int = 0
    def avg: Int = 0
    def maxDistance: Int = 0
    def maxAdjacentDistance: Int = -1
    def max: Int = 0
    def min: Int = 0
    def filterEven: IntList = Empty
    def filter(predicate: Int => Boolean): IntList = Empty
    def map(f: Int => Int): IntList = Empty
    def fold(start: Int, f: (Int, Int) => Int): Int = throw new Error("cannot fold empty")
    def forAll(predicateFunc: Int => Boolean): Boolean = true
}

case class Node(val head: Int, val tail: IntList) extends IntList {
    def isEmpty = false
    def contains(x: Int): Boolean = if(x == head) true else tail.contains(x)
    def get(index: Int): Int = if(index == 0) head else tail.get(index - 1)
    def delete(x: Int): IntList = if(head == x) tail else new Node(head, tail.delete(x))
    def deleteAll(x: Int): IntList = if(head == x) tail.delete(x) else new Node(head, tail.delete(x))
    def prefix(other: IntList): IntList = if(other.isEmpty) this else new Node(other.head, this.prefix(other.tail))
    def flip: IntList = if(isEmpty) Empty else new Node(head, Empty).prefix(tail.flip)
    def size: Int = if(this.isEmpty) Empty.size else 1 + this.tail.size
    def avg: Int = {
        def f(x: IntList, value: Int = 0, counter: Int = 0): Int = if(x.isEmpty) value / counter else f(x.tail, x.head + value, counter + 1)
        f(this)
    }
    def maxDistance: Int = {
        def f(x: IntList, min: Int, max: Int): Int = x match{
            case Empty => max - min
            case Node(head, tail) if(head < min && head < max) => f(tail, head, max)
            case Node(head, tail) if(head > max && head > min) => f(tail, min, head)
            case Node(head, tail) => f(tail, min, max)
        }
        f(this, this.head, this.head)
    }
    def maxAdjacentDistance: Int = {
        def f(distance: Int, x: IntList): Int = x match {
            case Node(_, Empty) => distance
            case Node(x, Node(y, tail)) if (math.abs(x - y) > distance) => f(math.abs(x - y), Node(y, tail))
            case Node(_, tail) => f(distance, tail)
        }
        def h(x: IntList): Int = x match{
            case Empty => Empty.maxAdjacentDistance
            case Node(_, Empty) => Empty.maxAdjacentDistance
            case Node(x, Node(y, tail)) => f(math.abs(x - y), Node(y, tail))
        }
        h(this)
    }
    def max: Int = {
        def f(x: IntList, currentMax: Int): Int = if(x.isEmpty) currentMax else if(x.head > currentMax) f(x.tail, x.head) else f(x.tail, currentMax)
        f(this, this.head)
    }
    def min: Int = {
        def f(x: IntList, currentMin: Int): Int = if(x.isEmpty) currentMin else if(x.head < currentMin) f(x.tail, x.head) else f(x.tail, currentMin)
        f(this, this.head)
    }
    def filterEven: IntList= {
        def f(x: IntList): IntList = x match {
            case Empty => Empty
            case Node(head, tail) if(head % 2 == 0) => new Node(head, f(tail))
            case Node(_, tail) => f(tail)
        }
        f(this)
    }
    def filter(predicate: Int => Boolean): IntList = {
        def f(x: IntList): IntList = x match {
            case Empty => Empty
            case Node(head, tail) if(predicate(head)) => new Node(head, f(tail))
            case Node(_, tail) => f(tail)
        }
        f(this)
    }
    def map(function: Int => Int) = {
        def f(x: IntList): IntList = x match{
            case Empty => Empty.map(function)
            case Node(head, tail) => new Node(function(head), f(tail))
        }
        f(this)
    }
    override def toString = s"$head $tail"
    // auswertung ist von rechts nach links, nicht tailrecursive !
    def fold(start: Int, f: (Int, Int) => Int): Int = {
        def h(list: IntList): Int = list match {
            case Empty => start
            case Node(head, tail) => f(head, h(tail))
        }
        h(this)
    }

    def forAll(predicateFunc: Int => Boolean): Boolean = {
        def f(current: IntList): Boolean = current match {
            case Empty => true
            case Node(head, tail) if(!predicateFunc(head)) => false
            case Node(head, tail) => f(tail)
        }
        f(this)
    }
}

// def apply(x: Int*): IntList = x match {
//     case Seq() => Empty
//     case _ => Node(x.head, this.apply(x.tail))
// }

def groupByV1[T, U](in: Iterable[T], f: T => U): Map[U, List[T]] = {
    val res = scala.collection.mutable.Map[U, List[T]]()
    for(e <- in) {
        val groupByValue = f(e)
        res.update(groupByValue, e::res.getOrElse(groupByValue, List()))
    }
    res.toMap.mapValues(_.reverse)
}

def groupByV2[T, U](in: Iterable[T], f: T => U): Map[U, List[T]] = {
    var res = Map[U, List[T]]()
    for(el <- in) {
        val groupByValue = f(el)
        res = res.updated(groupByValue, el::res.getOrElse(groupByValue, List()))
    }
    res.mapValues(_.reverse)
}

def groupByV3[T, U](in: Iterable[T], f: T => U): Map[U, List[T]] = {
    in.foldLeft(Map.empty[U, List[T]]) {
        (map, t) => 
            val groupByVal = f(t)
            map.updated(groupByVal, t::map.getOrElse(groupByVal, List.empty))
    }.mapValues(_.reverse)
}

def flatten(l: List[Any]): List[Any] = l match {
    case Nil => Nil
    case (head: List[_])::tail => flatten(head):::flatten(tail)
    case head::tail => head::flatten(tail)
}

def VorlesungsFlatten

val test = List(List(1, List(2), 4, List(), List(5)))
flatten(test)

// auswertung ist von links nach rechts
@tailrec // !!!!
final def vorlesungFold(start: Int, l: IntList, f: (Int, Int) => Int): Int = l match {
    case Empty => start
    case Node(head, tail) => vorlesungFold(f(start, head), tail, f)
}

// val list = apply(2, 4, 8, 24, 18, 16)
// list.filter(x => x % 2 == 0).fold(0, (a: Int, b: Int) => a + b)
// vorlesungFold(0, list.filter(x => x % 2 == 0), (a: Int, b: Int) => a + b)
// list.forAll(x => x % 2 == 0)
