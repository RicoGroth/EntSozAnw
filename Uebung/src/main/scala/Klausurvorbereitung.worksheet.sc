abstract class IntList {
  def isEmpty: Boolean
  def head: Int
  def tail: IntList
  def prefix(elem: IntList): IntList
  def flip: IntList
  def changeNumber(pred: Int => Boolean, change: Int => IntList): IntList
}

case object Empty extends IntList {
  def isEmpty = true
  def head = throw new Error("List is Empty")
  def tail = throw new Error("List is Empty")
  def prefix(elem: IntList): IntList = Empty
  def flip: IntList = Empty
  def changeNumber(pred: Int => Boolean, change: Int => IntList): IntList = Empty
}

case class Cons(head: Int, tail: IntList) extends IntList {
  def isEmpty = false
  def prefix(elem: IntList): IntList = elem match {
    case Empty      => this
    case Cons(h, t) => Cons(h, prefix(t))
  }
  def flip: IntList = if(isEmpty) Empty else new Cons(head, Empty).prefix(tail.flip)
  def changeNumber(pred: Int => Boolean, change: Int => IntList): IntList = this match {
    case Cons(head, tail) if(pred(head) == true) => Cons(head, tail.changeNumber(pred, change))
  }
}

val list = Cons(1, Cons(2, Cons(3, Cons(4, Empty))))
list.flip
