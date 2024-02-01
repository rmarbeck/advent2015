import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day13")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val happinessVariations = inputLines.map:
      case s"$from would $gainOrLose $variation happiness units by sitting next to $to." =>
        gainOrLose match
          case value if value == "gain" => HappinessVariation(from, to, variation.toInt)
          case _ => HappinessVariation(from, to, -variation.toInt)

    val part1Table = Table(happinessVariations)
    val resultPart1 = part1Table.getBestScore

    val happinessVariationsWithMyself = part1Table.friends.flatMap(currentFriend => List(HappinessVariation(currentFriend.name, "Myself", 0), HappinessVariation("Myself", currentFriend.name, 0))) ++ happinessVariations
    val part2Table = Table(happinessVariationsWithMyself)
    val resultPart2 = part2Table.getBestScore

    val result1 = s"${resultPart1}"
    val result2 = s"${resultPart2}"

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
end Solver

case class Table(variations: Seq[HappinessVariation]):
  import scala.collection.immutable.HashMap
  val friends: Seq[Friend] = variations.flatMap(_.friends).distinct
  val variationsScores: Map[String, Int] = HashMap(variations.map(variation => variation.couple -> variation.gain):_*)

  def getBestScore: Int =
    friends.permutations.map:
      case listOfFriends => listOfFriends.zip(listOfFriends.tail :+ listOfFriends.head).map:
        case (friend1, friend2)  => variationsScores.get(friend1.inCouple(friend2)).get + variationsScores.get(friend2.inCouple(friend1)).get
      .sum
    .max
case class HappinessVariation(from: String, to: String, gain: Int):
  lazy val friends = List(from, to).map(Friend(_))
  lazy val couple = Friend(from).inCouple(Friend(to))

case class Friend(name: String):
  def inCouple(myFriend: Friend) = this.name + " with "+ myFriend.name