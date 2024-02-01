import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day16")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val sues = inputLines.map:
      case s"Sue $index: $criterias" =>
        val listOfCriteria = criterias.split(",").map:
          case criteria =>
            val duo = criteria.split(":")
            Criteria(duo(0).trim, duo(1).trim.toInt)
        Sue(index.toInt, listOfCriteria.toSeq)

    val tickerTape = List(("children", 3),
                          ("cats", 7),
                          ("samoyeds", 2),
                          ("pomeranians", 3),
                          ("akitas", 0),
                          ("vizslas", 0),
                          ("goldfish", 5),
                          ("trees", 3),
                          ("cars", 2),
                          ("perfumes", 1)).map(Criteria.apply)

    def extract(sueResult: Option[Sue]) = sueResult match
      case Some(sue) => sue.index
      case None => throw Exception("Not found")

    val resultPart1 = extract(sues.find(_.matchesPart1(tickerTape)))
    val resultPart2 = extract(sues.find(_.matchesPart2(tickerTape)))

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
end Solver

case class Criteria(name: String, value: Int)

case class Sue(index: Int, criterias: Seq[Criteria]):
  def matchesPart1(matchingCriterias: Seq[Criteria]): Boolean =
    ! matchingCriterias.exists:
      case Criteria(criteriaName, value) => this.criterias.find(_.name == criteriaName).exists(_.value != value)

  def matchesPart2(matchingCriterias: Seq[Criteria]): Boolean =
    matchingCriterias.forall:
      case Criteria(criteriaName, value) if criteriaName == "cats" || criteriaName == "trees" => this.criterias.find(_.name == criteriaName).map(_.value > value).getOrElse(true)
      case Criteria(criteriaName, value) if criteriaName == "pomeranians" || criteriaName == "goldfish" => this.criterias.find(_.name == criteriaName).map(_.value < value).getOrElse(true)
      case Criteria(criteriaName, value) => this.criterias.find(_.name == criteriaName).map(_.value == value).getOrElse(true)