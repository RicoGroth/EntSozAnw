import scala.collection.immutable.Map
import scala.collection.immutable.List

val temperaturen:List[(String,String,List[(Int,Double)])]= List(("01.12.2022","Wohnzimmer",List((0,19.2),(1,21.3),(2,22.2),(3,20.7))), ("01.12.2022","Schlafzimmer",List((0,16.5),(1,17.3),(2,17.2),(3,17.1))), ("02.12.2022","Wohnzimmer",List((0,19.3),(1,21.2), (3,21.1))), ("02.12.2022","Schlafzimmer",List((0,16.6),(1,17.3),(2,17.1),(3,17.2))), ("03.12.2022","Wohnzimmer",List((0,19.5),(1,21.3), (2,22.8),(3,21.2))), ("03.12.2022","Schlafzimmer",List((0,16.2),(1,17.2),(2,17.3),(3,17.1))))

def extractValues(l:List[(String,String,List[(Int,Double)])]):List[(String,String,Double)] = {
    l.flatMap(entry => entry._3.map(timeAndTemp => (entry._1, entry._2, timeAndTemp._2)))
}
extractValues(temperaturen)

def getHighestTemperature(l:List[(String,String,List[(Int,Double)])]):(String,String,Double) = {
    extractValues(l).reduce((result, entry) => if(entry._3 > result._3) entry else result)
}
getHighestTemperature(temperaturen)

def getAverageTemperaturPerRoom(l:List[(String,String,List[(Int,Double)])]):Map[String,Double] = {
    extractValues(l).foldLeft(Map.empty[String, (Int, Double)])(
        (map, entry) => {
            val current = map.getOrElse(entry._2, (1, 0.0))
            map.updated(entry._2, (current._1 + 1, current._2 + entry._3))
        }
    ).mapValues(numberAndTemp => numberAndTemp._2 / numberAndTemp._1)
}

getAverageTemperaturPerRoom(temperaturen)

type Set = Int => Boolean
def contains(elem:Int, set:Set):Boolean = set(elem)
def insert(elem:Int, set:Set):Set= x => (elem==x || set(x))

def extendSet(s: Set, pred: Int=> Boolean):Set = x => contains(x, s) || pred(x)

val set: Set = x => x % 2 == 0
set(3)
val newSet: Set = extendSet(set, x => x % 2 != 0 && x > 0)
newSet(2)







abstract class IntList{

   def isEmpty:Boolean
   def head:Integer
  def tail:IntList
  def prefix(elem:IntList):IntList= elem match {

      case Empty => this
      case Cons(h,t) => Cons(h, prefix(t))
   }

   def intervalToList(start: Int, End:Int):IntList= start match {      
     case End => Empty
     case _ if (start<End) => Cons(start, intervalToList(start+1,End))
     case _ => Cons(start, intervalToList(start-1,End))
      }

    def completeList:IntList= this match {
        case Empty => Empty
        case Cons(head, Empty) => this
        case Cons(h1, Cons(h2, tail)) => Cons(h2, tail).completeList.prefix(intervalToList(h1, h2))
    }
}


case object Empty extends IntList{

   def isEmpty = true
  def head= throw new Error ("List is Empty")
  def tail= throw new Error ("List is Empty")
}

case class Cons(head:Integer, tail:IntList) extends IntList{

def isEmpty= false
}

val l= Cons(2,Cons(5, Cons(1, Empty))).completeList 