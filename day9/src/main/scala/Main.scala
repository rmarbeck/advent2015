import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.collection.mutable

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day9")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val distances = inputLines.map:
      case s"$city1 to $city2 = $distance" => Distance(city1, city2, distance.toInt)

    val result = solveNaively(World(distances))

    val result1 = s"${result._1}"
    val result2 = s"${result._2}"

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


  private def solveNaively(world: World): (Int, Int) =
    given World = world
    val results = world.cities.values.toList.permutations.map(Result(_))
    results.foldLeft((Int.MaxValue, 0)):
      case (acc, result) =>
        val score = result.score
        (math.min(acc(0), score), math.max(acc(1), score))

end Solver


case class Result(cities: Seq[City])(using world: World):
  require(cities.length == world.size)
  lazy val score: Int = cities.sliding(2, 1).map:
    case List(city1: City, city2: City) => world.distance(city1, city2)
    case _ => throw Exception("Not managed")
  .sum

case class World(distances: Seq[Distance]):

  import scala.collection.immutable.HashMap

  private val distancesCached: Map[String, Int] = HashMap(distances.flatMap(dist => Seq(dist.cityOneName + dist.cityTwoName -> dist.distance, dist.cityTwoName + dist.cityOneName -> dist.distance)): _*)
  val cities: Map[String, City] = distances.flatMap(dist => Seq(dist.cityOneName, dist.cityTwoName)).distinct.map(cityName => cityName -> City(cityName)).toMap

  lazy val size = cities.size

  def distance(city1: City, city2: City): Int =
    distancesCached.get(city1.name + city2.name) match
      case Some(value) => value
      case None => throw Exception(s"Unfound distance $city1 $city2")

case class City(name: String)

case class Distance(cityOneName: String, cityTwoName: String, distance: Int)
