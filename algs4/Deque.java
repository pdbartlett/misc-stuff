import edu.princeton.cs.algs4.StdOut;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class Deque<Item> implements Iterable<Item> {
  private static class Node<T> {
    private T value;
    private Node<T> prev;
    private Node<T> next;
  }

  private Node<Item> first = null;
  private Node<Item> last = null;
  private int size = 0;

  public Deque() {                           // construct an empty deque
  }

  public boolean isEmpty() {                 // is the deque empty?
    return first == null;
  }

  public int size() {                        // return the number of items on the deque
    return size;
  }

  public void addFirst(Item item) {          // add the item to the front
    Node<Item> n = new Node<>();
    n.value = validateNewItem(item);
    n.next = first;
    if (first != null) first.prev = n;
    first = n;
    if (last == null) last = first;
  }

  public void addLast(Item item) {           // add the item to the end
    Node<Item> n = new Node<>();
    n.value = validateNewItem(item);
    n.prev = last;
    if (last != null) last.next = n;
    last = n;
    if (first == null) first = last;
  }

  private Item validateNewItem(Item i) {
    if (i == null) throw new NullPointerException();
    size++;
    return i;
  }

  public Item removeFirst() {                // remove and return the item from the front
    tryDecrementSize();
    Item i = first.value;
    first = first.next;
    if (first == null) last = null;
    else first.prev = null;
    return i;
  }

  public Item removeLast() {                 // remove and return the item from the end
    tryDecrementSize();
    Item i = last.value;
    last = last.prev;
    if (last == null) first = null;
    else last.next = null;
    return i;
  }

  private void tryDecrementSize() {
    if (size == 0) throw new NoSuchElementException();
    --size;
  }

  public Iterator<Item> iterator() {         // return an iterator over items in order from front to end
    return new Iterator<Item>() {
      private Node<Item> curr = first;
      public boolean hasNext() { return curr != null; }
      public Item next() {
        if (!hasNext()) throw new NoSuchElementException();
        Item i = curr.value;
        curr = curr.next;
        return i;
      }
      public void remove() { throw new UnsupportedOperationException(); }
    };
  }

  public static void main(String[] args) {   // unit testing
    Deque<Integer> dq = new Deque<>();
    dq.addFirst(1);
    dq.addLast(2);
    dq.addFirst(3);
    for (int i : dq) StdOut.println(i);
    check(2, dq.removeLast());
    check(3, dq.removeFirst());
    check(1, dq.removeLast());
  }

  private static void check(int exp, int act) {
    if (exp != act) throw new AssertionError("Expected " + exp + "; got " + act);
  }
}
