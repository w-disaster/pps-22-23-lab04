package u04lab

import org.junit.Assert.*
import org.junit.*
import u04lab.code.{Item, Warehouse}

class WarehouseTest:

  import u04lab.code.List.*
  import u04lab.code.Option.*
  import u04lab.code.Option

  var warehouse: Warehouse = Warehouse()

  val dellXps: Item = Item(33, "Dell XPS 15", cons("notebook", empty))
  val dellInspiron: Item = Item(34, "Dell Inspiron 13", cons("notebook", empty))
  val xiaomiMoped: Item = Item(35, "Xiaomi S1", cons("moped", cons("mobility", empty)))

  @Before def beforeAll() =
    warehouse = Warehouse()

  @Test def testContains() =
    assertFalse(warehouse.contains(dellXps.code))
    warehouse.store(dellXps) // side effect, add dell xps to the warehouse
    assertTrue(warehouse.contains(dellXps.code))

  @Test def testSearchItems() =
    warehouse.store(dellXps) // side effect, add dell xps to the warehouse
    warehouse.store(dellInspiron) // side effect, add dell inspiron to the warehouse
    warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
    assertEquals(warehouse.searchItems("mobility"), Cons(xiaomiMoped, Nil()))
    assertEquals(warehouse.searchItems("notebook"), Cons(dellXps, Cons(dellInspiron, Nil())))

  @Test def testRetreive() =
    warehouse.store(dellXps) // side effect, add dell xps to the warehouse
    assertEquals(warehouse.retrieve(11), None())
    assertEquals(warehouse.retrieve(dellXps.code), Some(dellXps))
    warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
    assertEquals(warehouse.retrieve(dellXps.code), None())

  @Test def testVariadicArg() =
    val asusVivo: Item = Item(36, "Asus Vivobook S530U", "notebook", "GeForce MX130")
    warehouse.store(asusVivo)
    val storedAsusVivo: Option[Item] = warehouse.retrieve(36)
    assertEquals(storedAsusVivo, Some(asusVivo))
    assertTrue(storedAsusVivo match
      case Some(a) => a.tags == Cons("notebook", Cons("GeForce MX130", Nil()))
      case None() => false
    )


