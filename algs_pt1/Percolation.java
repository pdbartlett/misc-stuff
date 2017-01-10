import edu.princeton.cs.algs4.WeightedQuickUnionUF;

public class Percolation {
  private final int n;
  private final boolean[][] opened;
  private final WeightedQuickUnionUF fuf;
  private final WeightedQuickUnionUF puf;

  public Percolation(int n) {                // create n-by-n grid, with all sites blocked
    if (n < 1) throw new IllegalArgumentException();
    this.n = n;
    opened = new boolean[n][n];
    fuf = new WeightedQuickUnionUF(bottom());
    puf = new WeightedQuickUnionUF(bottom() + 1);
  }

  private int id(int row, int col) { return (row - 1) * n + col; }
  private int top() { return id(1, 1) - 1; }
  private int bottom() { return id(n, n) + 1; }

  private void validate(int row, int col) {
    validate(row);
    validate(col);
  }

  private void validate(int i) {
    if (i < 1 || i > n) {
      throw new IndexOutOfBoundsException(String.valueOf(i));
    }
  }

  public void open(int row, int col) {       // open site (row, col) if it is not open already
    validate(row, col);
    if (!isOpen(row, col)) {
      opened[row - 1][col - 1] = true;
      if (row == 1) {
        unionBoth(id(row, col), top());
      } else if (isOpen(row - 1, col)) {
        unionBoth(id(row, col), id(row - 1, col));
      }
      if (row == n) {
        puf.union(id(row, col), bottom());
      } else if (isOpen(row + 1, col)) {
        unionBoth(id(row, col), id(row + 1, col));
      }
      if (col > 1 && isOpen(row, col - 1)) {
        unionBoth(id(row, col), id(row, col - 1));
      }
      if (col < n && isOpen(row, col + 1)) {
        unionBoth(id(row, col), id(row, col + 1));
      }
    }
  }

  private void unionBoth(int from, int to) {
    fuf.union(from, to);
    puf.union(from, to);
  }

  public boolean isOpen(int row, int col) {  // is site (row, col) open?
    validate(row, col);
    return opened[row - 1][col - 1];
  }

  public boolean isFull(int row, int col) {  // is site (row, col) full?
    validate(row, col);
    return fuf.connected(top(), id(row, col));
  }

  public boolean percolates() {              // does the system percolate?
    return puf.connected(top(), bottom());
  }

  public static void main(String[] args) {   // test client (optional)
    Percolation p1 = new Percolation(1);
    check(!p1.isOpen(1, 1), "Initially not open");
    check(!p1.percolates(), "Initially doesn't percolate");
    p1.open(1, 1);
    check(p1.isOpen(1, 1), "Open after open");
    check(p1.percolates(), "Does percolate when only site open");

    Percolation p4 = new Percolation(4);
    check(!p4.percolates(), "Initially doesn't percolate");
    openAndCheck(p4, 1, 1);
    openAndCheck(p4, 2, 1);
    openAndCheck(p4, 2, 2);
    openAndCheck(p4, 2, 3);
    openAndCheck(p4, 2, 4);
    openAndCheck(p4, 3, 4);
    p4.open(4, 4);
    check(p4.percolates(), "Does percolate eventually");
  }

  private static void check(boolean check, String msg) {
    if (!check) throw new AssertionError(msg);
  }

  private static void openAndCheck(Percolation p, int row, int col) {
    p.open(row, col);
    check(!p.percolates(), "Doesn't percolate after: " + row + ", " + col);
  }
}
