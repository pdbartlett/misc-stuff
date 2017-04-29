package sorted

trait Dimension[T] {
  def allNodes: Iterable[T]
}

trait Model0[V] {
  def data(): V
}

trait Model1[V, D1] {
  def data(nodes: Iterable[D1]): Iterator[Pair[Tuple1[D1], V]]
}

trait Model2[V, D1, D2] {
  def data(nodes1: Iterable[D1], nodes2: Iterable[D2]): Iterator[Pair[(D1, D2), V]]
}

trait CellBasedModel1[V, D1] extends Model1[V, D1] {
  def getCellValue(node: D1) : V
  override def data(nodes: Iterable[D1]) =
    for (node <- nodes.elements) yield Pair(Tuple1(node), getCellValue(node))
}
