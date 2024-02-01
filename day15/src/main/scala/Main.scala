import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day15")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =
    val ingredients = inputLines.map:
      case s"$name: capacity $capacity, durability $durability, flavor $flavor, texture $texture, calories $calories" => (name, List(capacity, durability, flavor, texture, calories).map(_.toInt)) match
        case (name, List(capacity, durability, flavor, texture, calories)) => Ingredient(name, capacity, durability, flavor, texture, calories)
        case _ => throw Exception("Not possible")

    val resultPart1 = dig(100, Mixture.empty, ingredients, Mixture.score)

    val resultPart2 = dig(100, Mixture.empty, ingredients, Mixture.scorePart2)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromResource(fileName)
    val lines = bufferedSource.getLines().toSeq
    lines match
      case Nil => ("", "")
      case _ => runOn(lines)

  def dig(toReach: Int, currentMixture: Mixture, remainingIngredients: Seq[Ingredient], scoring : Mixture => Int): Int =
    remainingIngredients match
      case Nil => throw Exception("Should not happen")
      case head :: Nil =>
        head.validRange(toReach, currentMixture, Nil).map:
          case quantityToAdd => scoring.apply(currentMixture.add(head, quantityToAdd))
        .maxOption.getOrElse(0)
      case ingredients =>
        val (shortestRangeIngredient, shortestRange) = ingredients.map(ingredient => (ingredient, ingredient.validRange(toReach, currentMixture, remainingIngredients.filterNot(_ == ingredient)))).sortBy(_._2.size).head
        val newRemainingIngredients = remainingIngredients.filterNot(_ == shortestRangeIngredient)
        shortestRange.map:
          case quantity => dig(toReach, currentMixture.add(shortestRangeIngredient, quantity), newRemainingIngredients, scoring)
        .maxOption.getOrElse(0)

end Solver

case class Mixture(dozedIngredients: Seq[(Ingredient, Int)]):
  def add(ingredient: Ingredient, quantity: Int): Mixture = Mixture((ingredient, quantity) +: dozedIngredients)

  val teaspoons = dozedIngredients.map(_._2).sum
  val ingredients = dozedIngredients.map(_._1)
  lazy val scorePart2 =
    calories match
      case value if value == 500 => score
      case _ => 0
  lazy val score =
    values.dropRight(1).map:
      case value if value < 0 => 0
      case value => value
    .product
  lazy val values = dozedIngredients.foldLeft(List(0, 0, 0, 0, 0)):
    case (acc, (ingredient, quantity)) => ingredient.values.map(_ * quantity).zip(acc).map((newVal, currentVal) => newVal + currentVal)
  lazy val capacity = values(0)
  lazy val durability = values(1)
  lazy val flavor = values(2)
  lazy val texture = values(3)
  lazy val calories = values(4)

object Mixture:
  def score(mixture: Mixture) = mixture.score
  def scorePart2(mixture: Mixture) = mixture.scorePart2

  def empty: Mixture = Mixture(Nil)

case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int):
  def values = List(capacity, durability, flavor, texture, calories)

  def validRange(maxToReach: Int, currentMixture: Mixture, availableIngredients: Seq[Ingredient]): Range =
    val maxToAdd = maxToReach - currentMixture.teaspoons
    availableIngredients match
      case Nil => (maxToAdd to maxToAdd)
      case _ =>
        var (minRange: Int, maxRange: Int) = (0, maxToAdd)

        values.zipWithIndex.foreach:
          case (currentValue, index) =>
            currentValue match
              case value if value < 0 =>
                val currentValueInMixture = currentMixture.values(index)
                val maximumFromAvailable = availableIngredients.map(_.values(index)).max
                val xMax = (value / (1d + maximumFromAvailable)) * (currentValueInMixture + maxToAdd * maximumFromAvailable) / value
                loggerAOCPart1.trace(s"xMax = $xMax ($value - $maximumFromAvailable - $currentValueInMixture)")
                maxRange = math.min(maxRange, xMax.toInt)
              case value if value > 0 =>
                val currentValueInMixture = currentMixture.values(index)
                val minimumFromAvailable = availableIngredients.map(_.values(index)).min
                if (minimumFromAvailable * maxToAdd + currentValueInMixture < 0)
                  val xMin = -(currentValueInMixture + maxToAdd * minimumFromAvailable) / (value - minimumFromAvailable)
                  loggerAOCPart1.trace(s"xMin = $xMin ($value - $minimumFromAvailable - $currentValueInMixture)")
                  minRange = math.max(minRange, xMin.toInt)
              case 0 => ()

        minRange to maxRange

