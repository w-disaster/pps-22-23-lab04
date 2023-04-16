package u04lab

import org.junit.*
import org.junit.Assert.{assertEquals, assertTrue}
import u04lab.polyglot.Pair
import u04lab.polyglot.World
import u04lab.code.Option.*
import u04lab.polyglot.utils.{OptionToOptional, WorldUtils}

import java.util.Optional

class WorldTest:

  val size: Int = 4
  val mines: Int = 4
  var world: World = World(size, mines)

  @Before def beforeAll() =
    world = World(size, mines)

  @Test def testMinesPlacement() =
    assertEquals(WorldUtils.filterMapMineCells(world.getCells).size(), mines)

  @Test def testEmptyCells() =
    assertEquals(WorldUtils.filterMapEmptyCells(world.getCells, true).stream()
      .filter(c => c.getX.getX < size && c.getX.getX >= 0 &&
        c.getX.getY < size && c.getX.getY >= 0)
      .filter(c => c.getY.isEmpty)
      .count(), (size * size) - mines)

  @Test def testDisableEmptyCell() =
    val enabledCells: java.util.List[Pair[Pair[Integer, Integer], Optional[Integer]]] =
      WorldUtils.filterMapEmptyCells(world.getCells, true)
    val cell: Pair[Integer, Integer] = enabledCells.get(0).getX

    val value: Int = 10
    world.disable((cell.getX.intValue(), cell.getY.intValue()), value)

    val disabledCells: java.util.List[Pair[Pair[Integer, Integer], Optional[Integer]]] =
      WorldUtils.filterMapEmptyCells(world.getCells, false)
    assertTrue(disabledCells.contains(Pair(cell, OptionToOptional(Some(value)))))
