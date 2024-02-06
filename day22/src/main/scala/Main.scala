import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day22")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val boss = inputLines.mkString(", ") match
      case s"Hit Points: $hits, Damage: $damage" => new Player(hits.toInt, damage.toInt, 0)
      case _ => throw Exception("Not supported")

    val player = boss.hits.number match
      case value if value < 20 => new Player(10, 0, 250)
      case _ => new Player(50, 0, 500)

    val startingStrategy = Strategy(Nil, player, boss)

    val resultPart1 = findLowestCost(TreeSet(startingStrategy))
    val resultPart2 = findLowestCost(TreeSet(startingStrategy), isPart2 = true)

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

@tailrec
def findLowestCost(strategies: TreeSet[Strategy], bestFound: Int = Int.MaxValue, isPart2: Boolean = false): Int =
  strategies.size match
    case 0 => bestFound
    case _ =>
      val (head, tail) = (strategies.head, strategies.tail)
      strategies.map(_.cost).minOption match
        case Some(value) if value >= bestFound => bestFound
        case _ =>
          head.isAWin match
            case true => loggerAOCPart1.debug(s"found ${head.toString}");  findLowestCost(tail, math.min(bestFound, head.cost), isPart2)
            case false =>
              loggerAOCPart1.trace(s"continue ${head.toString}")
              isPart2 match
                case true => findLowestCost(tail ++ head.nextPart2, bestFound, isPart2)
                case false => findLowestCost(tail ++ head.next, bestFound, isPart2)


case class Hits(number: Int)
case class Damage(number: Int)
case class Mana(number: Int)

case class Player(hits: Hits, damage: Damage, mana: Mana):
  def this(hits: Int, damage: Int, mana: Int) = this(Hits(hits), Damage(damage), Mana(mana))

case class Strategy(decisions: List[Spell], player: Player, boss: Player) extends Ordered[Strategy]:
  def spellsNames: String = decisions.map(_.name).mkString
  override def equals(obj: Any): Boolean =
    obj match
      case Strategy(objDecisions, objPlayer, objBoss) => decisions.equals(objDecisions) && player.equals(objPlayer) && boss.equals(objBoss)
      case _ => false

  def next: Set[Strategy] =
    List[Spell](MagicMissile(), Drain(), Shield(), Poison(), Recharge()).filter(_.cost <= manaRemaining).filterNot(_.isStillActiveIn(this)).map(spell => Strategy(decisions :+ spell, player, boss)).filterNot(_.isALoss).toSet

  def nextPart2: Set[Strategy] =
    playerHitsRemaining > 1 match
      case false => Set()
      case true => List[Spell](MagicMissile(), Drain(), Shield(), Poison(), Recharge()).filter(_.cost <= manaRemaining).filterNot(_.isStillActiveIn(this)).map(spell => Strategy(decisions :+ spell, player.copy(hits = Hits(player.hits.number - 1)), boss)).filterNot(_.isALoss).toSet

  override def compare(that: Strategy): Int =
    bossHitsRemaining.compare(that.bossHitsRemaining) match
      case 0 => spellsNames.compare(that.spellsNames)
      case value => value
  lazy val turnsPlayed = 2 * decisions.size
  lazy val cost = decisions.map(_.cost).sum
  lazy val isAWin: Boolean = bossHitsRemaining <= 0
  lazy val isALoss: Boolean = playerHitsRemaining <= 0 && !isAWin
  def effectingTurns(spell: Spell, indexInList: Int): Int =
    val result = math.min((turnsPlayed - 2 * indexInList) - 1, spell.duration)
    spell match
      case poison: Poison => loggerAOCPart1.trace(s"Effecting Turns [$spell] out of [$turnsPlayed] [$indexInList] => $result")
      case _ => ()
    result

  def remainingTurnsAtEnd(test: Spell => Boolean): Int = decisions.zipWithIndex.findLast:
    case (spell, index) if test.apply(spell) => true
    case _ => false
  .map((spell, index) => spell.duration - effectingTurns(spell, index) - 1).getOrElse(0)

  lazy val manaRemaining: Int =
    decisions.zipWithIndex.map:
      case (spell: Recharge, index) => spell.manaGeneration * effectingTurns(spell, index)
      case _ => 0
    .sum - cost + player.mana.number
  lazy val playerHitsRemaining: Int =
    val naturalLoss = turnsPlayed / 2 * boss.damage.number
    val gains = decisions.zipWithIndex.map:
      case (spell: Drain, index) => spell.healing * effectingTurns(spell, index)
      case (spell: Shield, index) => math.min(spell.armor, boss.damage.number - 1) * ((effectingTurns(spell, index) + 1) / 2)
      case _ => 0
    .sum
    player.hits.number + gains - naturalLoss
  lazy val bossHitsRemaining: Int =
    val damages = decisions.zipWithIndex.map:
      case (spell: (MagicMissile | Drain | Poison), index) => spell.damages * effectingTurns(spell, index)
      case _ => 0
    .sum
    //println(s"$decisions => Damages for boss $damages")
    boss.hits.number - damages

  lazy val shieldRemainingTime: Int =
    remainingTurnsAtEnd:
      case current: Shield => true
      case _ => false
  lazy val poisonRemainingTime: Int =
    remainingTurnsAtEnd:
      case current: Poison => true
      case _ => false
  lazy val rechargeRemainingTime: Int =
    remainingTurnsAtEnd:
      case current: Recharge => true
      case _ => false

  override def toString: String = s"$decisions [$cost]: $isAWin ($manaRemaining, $bossHitsRemaining, $playerHitsRemaining, $shieldRemainingTime, $poisonRemainingTime, $rechargeRemainingTime)"

sealed trait Spell(val name: String, val cost: Int, val duration: Int, val damages: Int, val healing: Int, val armor: Int, val manaGeneration: Int):
  def isStillActiveIn(strategy: Strategy): Boolean = false

  def toStringDetailed: String = s"$this : $cost - $duration - $damages - $healing - $armor - $manaGeneration"
  override def toString: String = s"$name"

class MagicMissile extends Spell("MagicMissile", 53, 1, 4, 0, 0, 0)
class Drain extends Spell("Drain", 73, 1, 2, 2, 0, 0)
class Shield extends Spell("Shield", 113, 6, 0, 0, 7, 0):
  override def isStillActiveIn(strategy: Strategy): Boolean = strategy.shieldRemainingTime > 0
class Poison extends Spell("Poison", 173, 6, 3, 0, 0, 0):
  override def isStillActiveIn(strategy: Strategy): Boolean = strategy.poisonRemainingTime > 0
class Recharge extends Spell("Recharge", 229, 5, 0, 0, 0, 101):
  override def isStillActiveIn(strategy: Strategy): Boolean = strategy.rechargeRemainingTime > 0