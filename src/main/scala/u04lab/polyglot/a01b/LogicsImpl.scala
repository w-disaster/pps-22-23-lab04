package u04lab.polyglot.a01b
import u04lab.code.Option
import u04lab.code.Option.*
import u04lab.code.List
import u04lab.code.List.*
import u04lab.code.*
import u04lab.polyglot.a01b.Cell
import u04lab.polyglot.a01b.Cell.{Empty, Mine}
import scala.annotation.tailrec
import scala.util.Random

private enum Cell:
  case Mine(position: (Int, Int))
  case Empty(position: (Int, Int), o: Option[Int])

trait World:
  def getCells: List[Cell]
  def disable(p: (Int, Int), v: Int): Unit

object World:

  private val random: Random = Random(42)

  def apply(size: Int, mines: Int): World = WorldImpl(initCells(size, mines))

  private def initCells(size: Int, mines: Int): List[Cell] =
    ((m: List[Cell]) =>
      append(m, emptyCells((0, 0), (size, size), m)))(randomMines(Nil(), size, mines))

  private def emptyCells(topLeft: (Int, Int),
                         botRight: (Int, Int),
                         mines: List[Cell]): List[Cell] = topLeft match
    case t @ (tx, ty) => botRight match
      case (bx, by) if ty < by =>
          if tx < bx then
            if !contains(mines, Mine(t)) then
              Cons(Empty(t, None()), emptyCells((tx + 1, ty), botRight, mines))
            else
              emptyCells((tx + 1, ty), botRight, mines)
          else emptyCells((0, ty + 1), botRight, mines)
      case _ => Nil()
    case _ => Nil()

  private def randomMines(mines: List[Cell], size: Int, n: Int): List[Cell] = n match
    case 0 => mines
    case _ =>
      ((r: (Int, Int)) =>
        if contains(mines, Mine(r)) then
          randomMines(mines, size, n)
        else
          randomMines(append(mines, Cons(Mine(r), Nil())), size, n - 1))
        .apply((random.nextInt(size), random.nextInt(size)))

  private class WorldImpl(private var cells: List[Cell]) extends World:
    override def getCells: List[Cell] = cells

    override def disable(p: (Int, Int), v: Int): Unit =
      cells = map(cells) {
        case Empty(p1, None()) if p == p1 => Empty(p1, Some(v))
        case e @ Empty(_, _) => e
        case e @ Mine(_) => e
      }

class LogicsImpl(private val world: World) extends Logics:

  import Cell.*

  private def adjMines(p: (Int, Int)): List[Cell] =
    filter(world.getCells) {
      case Mine((x, y)) => p match
        case (px, py) if px != x || py != y => p match
        case (px, py) => Math.abs(px - x) <= 1 && Math.abs(py - y) <= 1
      case _ => false
    }

  @tailrec
  private def stepOn(l: List[(Int, Int)]): Unit = l match
    case Cons(h @ (hx, hy), t) => length(adjMines(h)) match
      case 0 =>
        world.disable(h, 0)
        stepOn(
          append(t,
            map(
              filter(world.getCells) {
                case Empty((x, y), None()) => Math.abs(hx - x) <= 1 && Math.abs(hy - y) <= 1 && (hx != x || hy != y)
                case _ => false
              }
            )
            { case Empty(p, None()) => p }
          )
        )
      case n =>
        world.disable(h, n)
        stepOn(t)
    case Nil() => {}

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    find(world.getCells) {
      case Empty((px, py), _) => px == x && py == y
      case Mine((px, py)) => px == x && py == y
    } match
      case Some(Mine(_)) => java.util.Optional.empty()
      case Some(Empty(p @ (_, _), None())) =>
        stepOn(Cons(p, Nil()))
        java.util.Optional.of(length(adjMines(p)))

  def won: Boolean = length(filter(world.getCells) {
    case Empty(_, None()) => true
    case _ => false
  }) == 0