package u04lab.polyglot

import u04lab.code.List.{Cons, Nil, append, contains, map}
import u04lab.code.{List, Option}
import u04lab.code.Option.{None, Some}
import u04lab.polyglot.Cell.{Empty, Mine}

import scala.util.Random

enum Cell:
  case Mine(position: (Int, Int))
  case Empty(position: (Int, Int), o: Option[Int])

trait World:
  def cells: List[Cell]
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

  private class WorldImpl(private var _cells: List[Cell]) extends World:
    override def cells: List[Cell] = _cells

    override def disable(p: (Int, Int), v: Int): Unit =
      _cells = map(_cells) {
        case Empty(p1, None()) if p == p1 => Empty(p1, Some(v))
        case e @ Empty(_, _) => e
        case e @ Mine(_) => e
      }
