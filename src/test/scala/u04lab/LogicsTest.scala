package u04lab

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test
import u04lab.polyglot.{Logics, Pair}
import u04lab.polyglot.a01b.LogicsImpl

import java.util.Optional

class LogicsTest:

  val size: Int = 4
  val mines: Int = 4
  var logics: Logics = LogicsImpl(size, mines)

  @Test def testWin() =
    var emptyCells: java.util.List[Pair[Pair[Integer, Integer], Optional[Integer]]] =
      logics.getEmptyCellsAsList(true)

    while (!emptyCells.isEmpty)
      logics.hit(emptyCells.get(0).getX.getX, emptyCells.get(0).getX.getY)
      emptyCells = logics.getEmptyCellsAsList(true)
    assertTrue(logics.isWon)

  @Test def testLoose() =
    val mineCells: java.util.List[Pair[Integer, Integer]] = logics.getMineCellsAsList
    assertEquals(logics.hit(mineCells.get(0).getX, mineCells.get(0).getY), Optional.empty())