package u04lab.polyglot
import u04lab.code.Option
import u04lab.code.Option.*
import u04lab.code.List
import u04lab.code.List.*
import u04lab.code.*
import u04lab.polyglot.{Logics, OptionToOptional, Pair}

import java.util
import java.util.Optional
import scala.annotation.tailrec
import scala.util.Random

class LogicsImpl(size: Int, mines: Int) extends Logics:

  import Cell.*
  private val world : World = World(size, mines)

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


  override def getMineCellsAsList: util.List[Pair[Integer, Integer]] =
    WorldUtils.filterMapMineCells(world.getCells)

  override def getEmptyCellsAsList(enabled: Boolean): util.List[Pair[Pair[Integer, Integer], Optional[Integer]]] =
    WorldUtils.filterMapEmptyCells(world.getCells, enabled)

  override def hit(x: Int, y: Int): java.util.Optional[Integer] =
    find(world.getCells) {
      case Empty((px, py), _) => px == x && py == y
      case Mine((px, py)) => px == x && py == y
    } match
      case Some(Mine(_)) => OptionToOptional(None())
      case Some(Empty(p @ (_, _), None())) =>
        stepOn(Cons(p, Nil()))
        OptionToOptional(Some(length(adjMines(p))))

  override def isWon: Boolean = length(filter(world.getCells) {
    case Empty(_, None()) => true
    case _ => false
  }) == 0