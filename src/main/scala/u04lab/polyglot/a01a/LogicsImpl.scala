package u04lab.polyglot.a01a
import Logics.*
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:

  val r = Random(42)
  private val x_p: Int = r.nextInt(size);
  private val y_p: Int = r.nextInt(size);
  private var missing: Int = boat;


  def hit(row: Int, col: Int) =
    if (x_p == col && y_p == row) then
      missing = missing - 1
      if (missing == 0) then
        Result.WON
      else Result.HIT
    else Result.MISS