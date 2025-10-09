object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val firstLineOfInstructions = inputLines.headOption.getOrElse("")

    val nbOfHousesSantaOnly = housesVisited(firstLineOfInstructions.iterator).size

    val (santa, roboSanta) = firstLineOfInstructions.zipWithIndex.partition(_._2 % 2 == 0)
    val List(santaHouses, roboSantaHouses) =
      List(santa, roboSanta).map(_.map(_._1).iterator).map(housesVisited)

    val nbOfHousesBoth = (santaHouses union roboSantaHouses).size

    (nbOfHousesSantaOnly.toString, nbOfHousesBoth.toString)

end Solution

type House = (Int, Int)

def housesVisited(instructions: Iterator[Char]): Set[House] =
  instructions.scanLeft((0, 0)):
    case ((curX, curY), '>') => (curX + 1, curY)
    case ((curX, curY), '<') => (curX - 1, curY)
    case ((curX, curY), '^') => (curX, curY + 1)
    case ((curX, curY), 'v') => (curX, curY - 1)
    case _ => throw Exception("Illegal input")
  .toSet