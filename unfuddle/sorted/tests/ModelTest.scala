package sorted

import org.scalatest.FunSuite

class ModelTest extends FunSuite {

  test("model0") {
    val model = new MockModel0
    assert(42 === model.data())
  }

  test("model1_A") {
    checkModel1Single("A", 1)
  }

  test("model1_B") {
    checkModel1Single("B", 2)
  }

  test("model1_C") {
    checkModel1Single("C", 3)
  }

  test("model1_null") {
    checkModel1Single(null, 6)
  }

  test("model1_AC") {
    checkModel1Multi(List("A", "C"), List(1, 3))
  }

  test("model1_allNodes") {
    val dim = new TestDim
    checkModel1Multi(dim.allNodes, List(1, 2, 3, 6))
  }

  test("model2_null_null") {
    val model = new MockModel2
    val data = model.data(List(null), List(null))
    val pair = data.next()
    assert((null, null) == pair._1)
    assert(9 === pair._2)
    assert(!data.hasNext)
  }

  test("model2_all_all") {
    val model = new MockModel2
    val dim = new TestDim
    val data = model.data(dim.allNodes, dim.allNodes)
    var pair = data.next()
    assert(("A", "A") == pair._1);   assert(1 === pair._2); pair = data.next()
    assert(("A", "B") == pair._1);   assert(1 === pair._2); pair = data.next()
    assert(("A", "C") == pair._1);   assert(1 === pair._2); pair = data.next()
    assert(("A", null) == pair._1);  assert(3 === pair._2); pair = data.next()
    assert(("B", "A") == pair._1);   assert(1 === pair._2); pair = data.next()
    assert(("B", "B") == pair._1);   assert(1 === pair._2); pair = data.next()
    assert(("B", "C") == pair._1);   assert(1 === pair._2); pair = data.next()
    assert(("B", null) == pair._1);  assert(3 === pair._2); pair = data.next()
    assert(("C", "A") == pair._1);   assert(1 === pair._2); pair = data.next()
    assert(("C", "B") == pair._1);   assert(1 === pair._2); pair = data.next()
    assert(("C", "C") == pair._1);   assert(1 === pair._2); pair = data.next()
    assert(("C", null) == pair._1);  assert(3 === pair._2); pair = data.next()
    assert((null, "A") == pair._1);  assert(3 === pair._2); pair = data.next()
    assert((null, "B") == pair._1);  assert(3 === pair._2); pair = data.next()
    assert((null, "C") == pair._1);  assert(3 === pair._2); pair = data.next()
    assert((null, null) == pair._1); assert(9 === pair._2); assert(!data.hasNext)
  }

  private def checkModel1Single(node: String, value: Int) {
    checkModel1Multi(List(node), List(value))
  }

  private def checkModel1Multi(nodes: List[String], values: List[Int]) {
    val model = new MockModel1
    val data = model.data(nodes)
    for ((node, value) <- nodes zip values) {
      val pair = data.next()
      assert(Tuple1(node) == pair._1)
      assert(value === pair._2)
    }
    assert(!data.hasNext)
  }
}

class TestDim extends Dimension[String] {
  def allNodes = List("A", "B", "C", null)
}

class MockModel0 extends Model0[Int] {
  override def data() = 42
}

class MockModel1 extends CellBasedModel1[Int, String] {
  private val dataMap = Map("A" -> 1, "B" -> 2, "C" -> 3)
  override def getCellValue(node: String) =
    if (node == null)
      (0 /: dataMap.values)(_+_)
    else
      dataMap(node)
}

class MockModel2 extends Model2[Int, String, String] {
  private def getValue(node1: String, node2: String) = (node1, node2) match {
    case (null, null) => 9
    case (null, _)    => 3
    case (_, null)    => 3
    case (_, _)       => 1
  }

  override def data(nodes1: Iterable[String], nodes2: Iterable[String]) =
    for (node1 <- nodes1.elements; node2 <- nodes2.elements)
      yield Pair((node1, node2), getValue(node1, node2))
}
