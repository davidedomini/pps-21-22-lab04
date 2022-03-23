package u04lab.polyglot.a01a
import Logics.*
import scala.util.Random

trait Pair:
  def firstElement: Int
  def secondElement: Int

trait PairSet:
  def addElement(e: Pair): Unit
  def size(): Int

object Pair:
  def apply(firstElem: Int, secondElem: Int): Pair = PairImpl(firstElem, secondElem)
  private case class PairImpl(override val firstElement: Int, override val secondElement: Int) extends Pair

object PairSet:
  def apply(): PairSet = PairSetImpl()

  private class PairSetImpl() extends PairSet:
    import u04lab.code.List
    private var elements: List[Pair] = List.Nil()
    private def checkNotAlreadyPresent(e: Pair): Boolean = !List.contains(elements, e)
    override def addElement(e: Pair): Unit = elements = if checkNotAlreadyPresent(e) then List.append(elements, List.Cons(e, List.Nil())) else elements
    override def size(): Int = List.length(elements)

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:

  private val hitsSet: PairSet = PairSet()
  private val maxFailures: Int = 5
  private val boatLeftCol: Int = Random.nextInt(size - (boat - 1));
  private val boatRow: Int = Random.nextInt(size);
  private var failures: Int = 0;

  println(boatRow + " " + boatLeftCol)

  def hit(row: Int, col: Int) =
    if(row == boatRow && col >= boatLeftCol && col < boatLeftCol + boat) then
      hitsSet.addElement(Pair(row, col))
      if(hitsSet.size() == boat) then Result.WON else Result.HIT
    else
      failures = failures + 1
      if (failures == maxFailures) then Result.LOST else Result.MISS
