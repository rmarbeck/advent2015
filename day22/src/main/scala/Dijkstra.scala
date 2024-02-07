import scala.annotation.tailrec

case class Summit(strategy: Strategy):
  lazy val totalCost = strategy.cost
  lazy val lastCost = strategy.decisions.last.cost
  lazy val index = strategy.bossHitsRemaining + strategy.decisions.last.cost
  lazy val next = strategy.next.map(Summit.apply)
  lazy val nextPart2 = strategy.nextPart2.map(Summit.apply)
  lazy val isAWin = strategy.isAWin

class GraphOfStrategiesPart2(val elements: Seq[Summit]) extends Graph[Summit]:
  override def getElements: Seq[Summit] = elements
  override def weightBetween(first: Data[Summit], second: Data[Summit]): Long = second.getElement.strategy.decisions.last.cost
  override def getNeighboursOfIn(current: Data[Summit], potentialNeighbours: Seq[Data[Summit]]): Seq[Data[Summit]] =
    current.getElement.nextPart2.map(currentElement => potentialNeighbours.find(_.getElement == currentElement).get).toSeq
  override def insertNewNeighbours(current: Data[Summit], existing:Seq[Data[Summit]]): Seq[Data[Summit]] =
    current.getElement.nextPart2.map:
      case summit => existing.find(_.getElement == summit) match
        case Some(previousValue) => previousValue
        case None => Data(summit)
    .toSeq ++ existing.filterNot(currentElement => current.getElement.nextPart2.contains(currentElement.getElement))

  override def getBest(existing:Seq[Data[Summit]]): Data[Summit] = existing.sortBy(_.getElement.index).head

class GraphOfStrategies(val elements: Seq[Summit]) extends Graph[Summit]:
  override def getElements: Seq[Summit] = elements
  override def weightBetween(first: Data[Summit], second: Data[Summit]): Long = second.getElement.strategy.decisions.last.cost
  override def getNeighboursOfIn(current: Data[Summit], potentialNeighbours: Seq[Data[Summit]]): Seq[Data[Summit]] =
    current.getElement.next.map(currentElement => potentialNeighbours.find(_.getElement == currentElement).get).toSeq
  override def insertNewNeighbours(current: Data[Summit], existing:Seq[Data[Summit]]): Seq[Data[Summit]] =
    current.getElement.next.map:
      case summit => existing.find(_.getElement == summit) match
        case Some(previousValue) => previousValue
        case None => Data(summit)
    .toSeq ++ existing.filterNot(currentElement => current.getElement.next.contains(currentElement.getElement))

  override def getBest(existing:Seq[Data[Summit]]): Data[Summit] = existing.sortBy(_.getElement.index).head

trait Graph[T]:
  def getElements: Seq[T]
  def getListToExploreInitialisedFromStart(startingFrom: T): Seq[Data[T]] =
    val data = getElements.filterNot(_ == startingFrom).map(Data[T](_))
    Data(startingFrom, 0) +: data
  def weightBetween(first: Data[T], second: Data[T]): Long
  def getNeighboursOfIn(current: Data[T], potentialNeighbours: Seq[Data[T]]): Seq[Data[T]]
  def insertNewNeighbours(current: Data[T], existing:Seq[Data[T]]): Seq[Data[T]]
  def getBest(existing:Seq[Data[T]]): Data[T] = existing.sortBy(_.getCurrentDistance).head

class Data[T](element: T, private var currentDistance: Long = Long.MaxValue):
  def getElement = element
  private var precedingElement: Option[Data[T]] = None
  def getPreceding: Data[T] = precedingElement.get
  def getCurrentDistance: Long = currentDistance
  def updateDistanceAndPreceding(newDistance: Long, preceding: Data[T]): Unit =
    currentDistance = newDistance
    precedingElement = Some(preceding)

  override def toString: String = s"{$element, $getCurrentDistance}"

object Dijkstra:
  def solve[T](graph: Graph[T], startingFrom: T, matchingElement: T => Boolean): Int =
    doSolve[T](graph.getListToExploreInitialisedFromStart(startingFrom).toList, Nil, matchingElement)(graph)

  def solve[T](graph: Graph[T], startingFrom: T, elementsToReach: List[T]): Int =
    doSolve[T](graph.getListToExploreInitialisedFromStart(startingFrom).toList, Nil, elementsToReach)(graph)

  @tailrec
  private def doSolve[T](toExplore: List[Data[T]], explored: List[Data[T]], matchingElement: T => Boolean)(implicit graph: Graph[T]): Int =
    //println(s"${toExplore.headOption.map(_.getCurrentDistance)}")
    explored.find(current => matchingElement.apply(current.getElement)) match
      case Some(result) => result.getCurrentDistance.toInt
      case None =>
        toExplore match
          case Nil => println(s"Not found") ; Int.MaxValue
          case currentList =>
            val best = graph.getBest(toExplore)
            val tail = toExplore.filterNot(_ == best)
            val expandedExploringList = graph.insertNewNeighbours(best, tail)
            graph.getNeighboursOfIn(best, expandedExploringList).foreach: neighbour =>
              val distance = best.getCurrentDistance + graph.weightBetween(best, neighbour)
              if (neighbour.getCurrentDistance > distance)
                neighbour.updateDistanceAndPreceding(distance, best)

            doSolve(expandedExploringList.toList, best :: explored, matchingElement)


  @tailrec
  private def doSolve[T](toExplore: List[Data[T]], explored: List[Data[T]], elementsToReach: List[T])(implicit graph: Graph[T]): Int =
    toExplore match
      case Nil => rewind(explored.head)
      case _ if  ! (explored.map(_.getElement) intersect elementsToReach).isEmpty => rewind(explored.head)
      case best :: tail =>
        graph.getNeighboursOfIn(best, toExplore).foreach: neighbour =>
          val distance = best.getCurrentDistance + graph.weightBetween(best, neighbour)
          if (neighbour.getCurrentDistance > distance)
            neighbour.updateDistanceAndPreceding(distance, best)

        doSolve(tail.sortBy(_.getCurrentDistance), best :: explored, elementsToReach)

  @tailrec
  private def rewind[T](current: Data[T], counter: Int=0): Int =
    current.getCurrentDistance match
      case 0 => counter
      case _ => rewind(current.getPreceding, counter + 1)