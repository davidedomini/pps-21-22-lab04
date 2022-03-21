package u04lab.polyglot.a01a
import Logics.*
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:

  private var hits: Int = 0
  private val maxFailures: Int = 5
  private val boatLeftCol: Int = Random.nextInt(size - (boat - 1));
  private val boatRow: Int = Random.nextInt(size);
  private var failures: Int = 0;

  println(boatRow + " " + boatLeftCol)

  def hit(row: Int, col: Int) =
    if(row == boatRow && col >= boatLeftCol && col < boatLeftCol + boat) then
      hits = hits + 1
      if(hits == boat) then Result.WON else Result.HIT
    else
      failures = failures + 1
      if (failures == maxFailures) then Result.LOST else Result.MISS
