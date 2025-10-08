object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (wrappingPaper, ribbon) = inputLines.iterator.foldLeft((0, 0)):
      case (alreadyWrapped,  PresentExtractor(present)) => present addTo alreadyWrapped

    (wrappingPaper.toString, ribbon.toString)

end Solution

case class Present(length: Int, width: Int, height: Int):
  private val dimensions = Array(length, width, height)
  private val smallestDimensions = dimensions.sorted.take(2)
  private val wrappingPaper: Int = 2 * dimensions.combinations(2).map(_.product).sum + smallestDimensions.product
  private val ribbon: Int = dimensions.product + smallestDimensions.sum * 2

  def addTo(currentSize: (Int, Int)): (Int, Int) =
    (currentSize._1 + wrappingPaper, currentSize._2 + ribbon)

object PresentExtractor:
  def unapply(str: String): Option[Present] = str match
    case s"${l}x${w}x${h}" => Array(l, w, h).flatMap(_.toIntOption) match
      case Array(length, width, height) => Some(Present(length, width, height))
      case _ => None
