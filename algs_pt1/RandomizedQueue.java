import edu.princeton.cs.algs4.StdRandom;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class RandomizedQueue<Item> implements Iterable<Item> {

  private Item[] data = (Item[]) new Object[1];
  private int size = 0;

  public RandomizedQueue() {                 // construct an empty randomized queue
  }

  public boolean isEmpty() {                 // is the queue empty?
    return size == 0;
  }

  public int size() {                        // return the number of items on the queue
    return size;
  }

  private int capacity() {
    return data.length;
  }

  public void enqueue(Item item) {           // add the item
    if (item == null) throw new NullPointerException();
    if (size == capacity()) {
      Item[] bigger = (Item[]) new Object[size * 2];
      for (int i = 0; i < size; ++i) {
        bigger[i] = data[i];
      }
      data = bigger;
    }
    data[size++] = item;
  }

  public Item dequeue() {                    // remove and return a random item
    if (isEmpty()) throw new NoSuchElementException();
    int ix = StdRandom.uniform(size);
    Item item = data[ix];
    data[ix] = data[--size];
    data[size] = null;
    if (size < capacity() / 4) {
      Item[] smaller = (Item[]) new Object[capacity() / 2];
      for (int i = 0; i < size; ++i) {
        smaller[i] = data[i];
      }
      data = smaller;
    }
    return item;
  }

  public Item sample() {                     // return (but do not remove) a random item
    if (isEmpty()) throw new NoSuchElementException();
    return data[StdRandom.uniform(size)];
  }

  public Iterator<Item> iterator() {         // return an independent iterator over items in random order
    return new Iterator<Item>() {
      private Item[] shuffled;
      private int next = 0;
      {
        shuffled = (Item[]) new Object[size];
        for (int i = 0; i < size; ++i) shuffled[i] = data[i];
        StdRandom.shuffle(shuffled);
      }
      public boolean hasNext() { return next < shuffled.length; }
      public Item next() {
        if (!hasNext()) throw new NoSuchElementException();
        return shuffled[next++];
      }
      public void remove() { throw new UnsupportedOperationException(); }
    };
  }

  public static void main(String[] args) {   // unit testing
  }
}
