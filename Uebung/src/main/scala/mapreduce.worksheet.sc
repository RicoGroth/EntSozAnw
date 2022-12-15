import scala.collection.immutable.Map
def mapReduceV1[Input_T, Mapped_T, Result_T](
    mapFunction: Input_T => Mapped_T, 
    reduceFunction: (Result_T, Mapped_T) => Result_T, 
    base: Result_T, 
    list: List[Input_T]
    ): Result_T = {
        list.map(mapFunction).foldLeft(base)(reduceFunction)
}

def tokenize(text: String): List[String] = text.toLowerCase.replaceAll("[^a-z ä ö ü]", " ").split(" ").filter(_ != "").toList
def mapTokenToValue(token: String): (String, Int) = (token, 1)
def addTokenToResult(map: Map[String, Int], tokenAndValue: (String, Int)): Map[String, Int] = {
    map.updated(tokenAndValue._1, tokenAndValue._2 + map.getOrElse(tokenAndValue._1, 0))
}

val inputV1 = """
Die Hochschule für Technik und Wirtschaft Berlin (HTW Berlin) ist mit fast 
14.000 Studierenden
und über 500 Beschaeftigten die groesste staatliche Fachhochschule Berlins 
und Ostdeutschlands. 
Es existieren etwa 70 Studienangebote in den Bereichen Technik, Informatik, 
Wirtschaft, Kultur und Gestaltung.
Die HTW Berlin verteilt sich auf zwei Standorte: den Campus Treskowallee in 
Berlin-Karlshorst und den 
Campus Wilhelminenhof in Berlin-Oberschoeneweide.
"""

val outputV1 = mapReduceV1(
    mapTokenToValue,
    addTokenToResult,
    Map.empty[String, Int],
    tokenize(inputV1)
)

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//       Mit mapReduceV1 ist keine Parallelisierung moeglich. Deshalb muessen als erstes der map Teil und das Inputdatenformat angepasst werden.       //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

def mapReduceV2[Input_T, Mapped_T, Result_T](
    mapFunction: Input_T => List[Mapped_T],                         // mapFunction gibt jetzt eine Liste zurueck...
    reduceFunction: (Result_T, Mapped_T) => Result_T, 
    base: Result_T, 
    list: List[Input_T]
    ): Result_T = {
        list.flatMap(mapFunction).foldLeft(base)(reduceFunction)    // ...die dann hier flachgezogen wird, reduceFunction bleibt gleich
}

// Format: (Zeilennummer, Zeile), Die Zeilennummer agiert hier als Schluessel und ist sonst nicht weiter wichtig
val inputV2 = List(
    (1, "Die Hochschule für Technik und Wirtschaft Berlin (HTW Berlin) ist mit fast 14.000 Studierenden"),
    (2, "und über 500 Beschaeftigten die groesste staatliche Fachhochschule Berlins und Ostdeutschlands."),
    (3, "Es existieren etwa 70 Studienangebote in den Bereichen Technik, Informatik, Wirtschaft, Kultur und Gestaltung."),
    (4, "Die HTW Berlin verteilt sich auf zwei Standorte: den Campus Treskowallee in Berlin-Karlshorst und den"),
    (5, "Campus Wilhelminenhof in Berlin-Oberschoeneweide.")
)

def mapValueToWordsInLine(line: (Int, String)): List[(String, Int)] = {
    line._2
    .toLowerCase()
    .replaceAll("[^a-z ö ä ü]", " ")
    .split(" ")
    .filter(_ != "")
    .toList
    .map(word => (word, 1))
}

val outputV2 = mapReduceV2(
    mapValueToWordsInLine,
    addTokenToResult,
    Map.empty[String, Int],
    inputV2
)

assert(outputV1.equals(outputV2))

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////       Uebung mit Primzahlzerlegung       ////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
val primeInput = List(24, 12, 5)
// 2 was counted 5 times as prime factor in the provided list, 3 was counted 2 times and 5 was counted 1 time
val expectedOutput = Map((2 -> 5), (3 -> 2), (5 -> 1))

def mappingStage(number: Int): List[Int] = List.empty[Int] // TODO:
def reductionStage(map: Map[Int, Int], primeFactor: Int): Map[Int, Int] = Map.empty[Int, Int] // TODO:

val primeOutput = mapReduceV2(
    mappingStage,
    reductionStage,
    Map.empty[Int, Int],
    primeInput
)

assert(primeOutput.equals(expectedOutput))