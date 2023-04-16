package u04lab.polyglot.utils

import u04lab.code.List.*
import u04lab.code.{List, Option}
import u04lab.code.Option.*
import u04lab.polyglot.Cell.{Empty, Mine}
import u04lab.polyglot.{Cell, Pair, World}

object WorldUtils:

  def filterMapMineCells(cells: List[Cell]): java.util.List[Pair[Integer, Integer]] = cells match
    case Cons(h, t) => h match
      case Mine((x, y)) => java.util.stream.Stream
        .concat(java.util.stream.Stream.of(Pair(Int.box(x), Int.box(y))), filterMapMineCells(t).stream())
        .toList
      case _ => java.util.List.of()
      case Nil() => java.util.List.of()

  def filterMapEmptyCells(cells: List[Cell], enabled: Boolean):
  java.util.List[Pair[Pair[Integer, Integer], java.util.Optional[Integer]]] = cells match
    case Cons(h, t) => h match
      case Empty((x, y), o) => o match
        case Some(a) if !enabled =>
          java.util.stream.Stream
            .concat(
              java.util.stream.Stream.of(
                Pair(Pair(Int.box(x), Int.box(y)), java.util.Optional.of(Int.box(a)))
              ),
              filterMapEmptyCells(t, enabled).stream()
            )
            .toList
        case None() if enabled =>
          java.util.stream.Stream
            .concat(
              java.util.stream.Stream.of(
                Pair(Pair(Int.box(x), Int.box(y)), java.util.Optional.empty())
              ),
              filterMapEmptyCells(t, enabled).stream()
            )
            .toList
        case _ => filterMapEmptyCells(t, enabled)
      case _ => filterMapEmptyCells(t, enabled)
    case Nil() => java.util.List.of()