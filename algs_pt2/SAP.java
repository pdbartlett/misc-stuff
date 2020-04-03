import edu.princeton.cs.algs4.BreadthFirstDirectedPaths;
import edu.princeton.cs.algs4.Digraph;
import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.StdIn;
import edu.princeton.cs.algs4.StdOut;

public class SAP {
  private final Digraph g;

  // constructor takes a digraph (not necessarily a DAG)
  public SAP(Digraph G) {
    g = new Digraph(G);
  }

  // length of shortest ancestral path between v and w; -1 if no such path
  public int length(int v, int w) {
    return singleHelper(v, w)[1];
  }

  // a common ancestor of v and w that participates in a shortest ancestral path; -1 if no such path
  public int ancestor(int v, int w) {
    return singleHelper(v, w)[0];
  }

  // length of shortest ancestral path between any vertex in v and any vertex in w; -1 if no such path
  public int length(Iterable<Integer> v, Iterable<Integer> w) {
    return multiHelper(v, w)[1];
  }

  // a common ancestor that participates in shortest ancestral path; -1 if no such path
  public int ancestor(Iterable<Integer> v, Iterable<Integer> w) {
    return multiHelper(v, w)[0];
  }

  private int[] singleHelper(int v, int w) {
    validateIndex(v);
    validateIndex(w);
    BreadthFirstDirectedPaths bfdp1 = new BreadthFirstDirectedPaths(g, v);
    BreadthFirstDirectedPaths bfdp2 = new BreadthFirstDirectedPaths(g, w);
    return helper(bfdp1, bfdp2);
  }

  private int[] multiHelper(Iterable<Integer> v, Iterable<Integer> w) {
    validateIndices(v);
    validateIndices(w);
    BreadthFirstDirectedPaths bfdp1 = new BreadthFirstDirectedPaths(g, v);
    BreadthFirstDirectedPaths bfdp2 = new BreadthFirstDirectedPaths(g, w);
    return helper(bfdp1, bfdp2);
  }

  private int[] helper(BreadthFirstDirectedPaths bfdp1, BreadthFirstDirectedPaths bfdp2) {
    int[] retval = new int[2];
    retval[0] = -1;
    retval[1] = Integer.MAX_VALUE;
    for (int i = 0; i < g.V(); ++i) {
      if (bfdp1.hasPathTo(i) && bfdp2.hasPathTo(i)) {
        int length = bfdp1.distTo(i) + bfdp2.distTo(i);
        if (length < retval[1]) {
          retval[1] = length;
          retval[0] = i;
        }
      }
    }
    if (retval[0] == -1) retval[1] = -1;
    return retval;
  }

  private void validateIndices(Iterable<Integer> is) {
    if (is == null) throw new IllegalArgumentException("Null iterable passed");
    for (int i : is) validateIndex(i);
  }

  private void validateIndex(int i) {
    if (i < 0 || i >= g.V()) throw new IllegalArgumentException("Invalid index: " + i);
  }

  // do unit testing of this class
  public static void main(String[] args) {
    In in = new In(args[0]);
    Digraph G = new Digraph(in);
    SAP sap = new SAP(G);
    while (!StdIn.isEmpty()) {
        int v = StdIn.readInt();
        int w = StdIn.readInt();
        int length   = sap.length(v, w);
        int ancestor = sap.ancestor(v, w);
        StdOut.printf("length = %d, ancestor = %d\n", length, ancestor);
    }
  }
}
