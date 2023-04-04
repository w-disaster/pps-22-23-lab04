package u04lab.polyglot.a01b

import u04lab.code.List
import u04lab.code.List.{Cons, Nil, filter}
import u04lab.code.Option.*
import u04lab.polyglot.a01b.Cell.{Empty, Mine}

object Utils:

  def getMineCellsAsList(cells: List[Cell]): java.util.List[u04lab.polyglot.Pair[Integer, Integer]] = cells match
    case Cons(h, t) => h match
      case Mine((x, y)) => java.util.stream.Stream
        .concat(java.util.stream.Stream.of(u04lab.polyglot.Pair(Int.box(x), Int.box(y))), getMineCellsAsList(t).stream())
        .toList
      case _ => java.util.List.of()
    case Nil() => java.util.List.of()

  def getEmptyCellsAsList(cells: List[Cell], enabled: Boolean):
  java.util.List[u04lab.polyglot.Pair[u04lab.polyglot.Pair[Integer, Integer], java.util.Optional[Integer]]] = cells match
    case Cons(h, t) => h match
      case Empty((x, y), o) => o match
          case Some(a) if !enabled =>
            java.util.stream.Stream
              .concat(
                java.util.stream.Stream.of(
                  u04lab.polyglot.Pair(u04lab.polyglot.Pair(Int.box(x), Int.box(y)), java.util.Optional.of(Int.box(a)))
                ),
                getEmptyCellsAsList(t, enabled).stream()
              )
            .toList
          case None() if enabled =>
            java.util.stream.Stream
              .concat(
                java.util.stream.Stream.of(
                  u04lab.polyglot.Pair(u04lab.polyglot.Pair(Int.box(x), Int.box(y)), java.util.Optional.empty())
                ),
                getEmptyCellsAsList(t, enabled).stream()
              )
            .toList
          case _ => getEmptyCellsAsList(t, enabled)
      case _ => getEmptyCellsAsList(t, enabled)
    case Nil() => java.util.List.of()
