object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val firstLineOfInput = inputLines.headOption.getOrElse("")

    val (floor, firstBasementPos, _) = firstLineOfInput.iterator.foldLeft((0, Option.empty[Int], 1)):
      case ((0, None, level), ')') => (-1, Some(level), level + 1)
      case ((floor, firstBasementPos, level), c) => (floor + c.direction, firstBasementPos, level + 1)

    (s"$floor", s"${firstBasementPos.getOrElse(-1)}")

end Solution

extension (parenthesis: Char)
  def direction: Int = parenthesis match
      case '(' => 1
      case ')' => -1
      case _ => 0
