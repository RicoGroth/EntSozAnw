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
  def changeNumber(pred: Int => Boolean, change: Int => IntList): IntList =
    Empty
}

case class Cons(head: Int, tail: IntList) extends IntList {
  def isEmpty = false
  def prefix(elem: IntList): IntList = elem match {
    case Empty      => this
    case Cons(h, t) => Cons(h, prefix(t))
  }
  def flip: IntList =
    if (isEmpty) Empty else new Cons(head, Empty).prefix(tail.flip)
  def changeNumber(pred: Int => Boolean, change: Int => IntList): IntList = {
    if (pred(head) == true) tail.changeNumber(pred, change).prefix(change(head))
    else Cons(head, tail.changeNumber(pred, change))
  }
}
Cons(1,Cons(3,Cons(2,Empty))).changeNumber(_%2==0, x=>Cons(x,Cons(x,Empty))) 
val list = Cons(1, Cons(3, Cons(2, Empty)))
list.flip
list.changeNumber(_ < 3, x => Cons(x, Cons(x, Empty)))

val calories: List[(String, String, List[(String, Int)])] =
  List(
    (
      "Donald Duck",
      "2022-01-01",
      List(
        ("Frühstück", 800),
        ("Mittag", 700),
        ("Snack", 200),
        ("Abendbrot", 500)
      )
    ),
    (
      "Donald Duck",
      "2022-01-02",
      List(("Frühstück", 700), ("Mittag", 650), ("Abendbrot", 520))
    ),
    (
      "Donald Duck",
      "2022-01-03",
      List(
        ("Frühstück", 800),
        ("Mittag", 700),
        ("Snack", 200),
        ("Abendbrot", 500),
        ("Snack", 150)
      ) // HERE
    ),
    (
      "Donald Duck",
      "2022-01-04",
      List(("Frühstück", 850), ("Mittag", 900), ("Snack", 500), ("Snack", 400))
    ),
    (
      "Donald Duck",
      "2022-01-05",
      List(
        ("Frühstück", 600),
        ("Mittag", 700),
        ("Snack", 200),
        ("Abendbrot", 100)
      )
    ),
    (
      "Dagobert Duck",
      "2022-01-01",
      List(
        ("Frühstück", 300),
        ("Mittag", 500),
        ("Snack", 100),
        ("Abendbrot", 200)
      )
    ),
    (
      "Dagobert Duck",
      "2022-01-02",
      List(
        ("Frühstück", 200),
        ("Mittag", 300),
        ("Snack", 400),
        ("Abendbrot", 200)
      )
    ),
    (
      "Dagobert Duck",
      "2022-01-03",
      List(("Frühstück", 800), ("Mittag", 700), ("Snack", 200), ("Snack", 200))
    ),
    (
      "Dagobert Duck",
      "2022-01-04",
      List(("Frühstück", 200), ("Mittag", 300), ("Snack", 200), ("Snack", 500))
    ),
    (
      "Dagobert Duck",
      "2022-01-05",
      List(("Frühstück", 200), ("Mittag", 700), ("Abendbrot", 500))
    )
  )

def dayWithMaxCalories(
    l: List[(String, String, List[(String, Int)])]
): (String, String, Int) =
  l.map(entry =>
    (
      entry._1,
      entry._2,
      entry._3.foldLeft(0)((carry, element) => carry + element._2)
    )
  ).reduceLeft((a, b) => if (a._3 > b._3) a else b)

dayWithMaxCalories(calories)

def caloriesByMeal(
    l: List[(String, String, List[(String, Int)])]
): Map[String, Int] =
  l.flatMap(_._3)
    .foldLeft(Map.empty[String, Int])((carry, entry) =>
      carry.updated(entry._1, carry.getOrElse(entry._1, 0) + entry._2)
    )

def test(l: List[(String, String, List[(String, Int)])]): List[Int] = l.map(entry => entry._3.map(caloriesEntry => caloriesEntry._2).sum)

def lunchCaloriesCombined(l: List[(String, String, List[(String, Int)])]): Int = {
  l.foldLeft(0)((sum, entry) => sum + entry._3
      .filter(caloriesByDaytime => caloriesByDaytime._1 == "Mittag")
      .foldLeft(0)((tempSum, lunchCaloriesEntry) => tempSum + lunchCaloriesEntry._2)
  )
}

def sumOfCaloriesByName(name: String, l: List[(String, String, List[(String, Int)])]): Int = {
  l.foldLeft(0)((sum, entry) => {
    if(entry._1 == name) sum + entry._3.foldLeft(0)(
      (daySum, daytimeCaloriesEntry) => daytimeCaloriesEntry._2 + daySum
    )
    else sum
  })
}

lunchCaloriesCombined(calories)
sumOfCaloriesByName("Donald Duck", calories)
sumOfCaloriesByName("Dagobert Duck", calories)

// def caloriesByMealAlternative(
//     l: List[(String, String, List[(String, Int)])]
// ): Map[String, Int] =
//   l.flatMap(_._3).groupBy(_._1).view.mapValues(value => value.map(_._2).sum).toMap

// caloriesByMealAlternative(calories)

// val n = List(1, 2, 3, 4, 5)
// val t = Map(1 -> Set(1, 2), 2 -> Set(3, 4, 1), 3 -> Set(1, 4), 4 -> Set(1, 2, 3))
// n.map(i => t.getOrElse(i, Set.empty)).reduceLeft((a, b) => if(b.isEmpty) a else a.intersect(b))