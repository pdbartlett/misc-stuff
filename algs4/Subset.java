import edu.princeton.cs.algs4.StdIn;
import edu.princeton.cs.algs4.StdOut;

public class Subset {
  public static void main(String[] args) {
    RandomizedQueue<String> q = new RandomizedQueue<>();
    while (!StdIn.isEmpty()) {
      q.enqueue(StdIn.readString());
    }
    for (int i = 0; i < Integer.parseInt(args[0]); ++i) {
      StdOut.println(q.dequeue());
    }
  }
}
