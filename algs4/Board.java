import java.util.ArrayList;
import java.util.Arrays;

public class Board {
  private final int[][] blocks;
  private int spaceRow;
  private int spaceCol;

  public Board(int[][] blocks) {           // construct a board from an n-by-n array of blocks
                                           // (where blocks[i][j] = block in row i, column j)
    if (blocks.length < 2) throw new IllegalArgumentException();
    this.blocks = copy(blocks);
    for (int i = 0; i < blocks.length; ++i) {
      if (blocks[i].length != blocks.length) throw new IllegalArgumentException();
      for (int j = 0; j < blocks.length; ++j) {
        if (blocks[i][j] == 0) {
          spaceRow = i;
          spaceCol = j;
        }
      }
    }
  }

  public int dimension() {                 // board dimension n
    return blocks.length;
  }

  public int hamming() {                   // number of blocks out of place
    int count = -1;
    int expected = 1;
    for (int i = 0; i < dimension(); ++i) {
      for (int j = 0; j < dimension(); ++j) {
        if (blocks[i][j] != expected++) ++count;
      }
    }
    return count;
  }

  public int manhattan() {                 // sum of Manhattan distances between blocks and goal
    int sum = 0;
    for (int i = 0; i < dimension(); ++i) {
      for (int j = 0; j < dimension(); ++j) {
        int expected = blocks[i][j] - 1;
        if (expected >= 0) {
          sum += Math.abs(expected / dimension() - i) + Math.abs(expected % dimension() - j);
        }
      }
    }
    return sum;
  }

  public boolean isGoal() {                // is this board the goal board?
    return hamming() == 0;
  }

  public Board twin() {                    // a board that is obtained by exchanging any pair of blocks
    if (blocks[0][0] == 0) {
      return twin(0, 1, 1, 1);
    }
    if (blocks[0][1] == 0) {
      return twin(0, 0, 1, 0);
    }
    return twin(0, 0, 0, 1);
  }

  private Board twin(int r1, int c1, int r2, int c2) {
    return exchange(r1, c1, r2, c2);
  }

  public boolean equals(Object y) {        // does this board equal y?
    if (y != null && y.getClass() == getClass()) {
      return Arrays.deepEquals(blocks, ((Board) y).blocks);
    }
    return false;
  }

  public Iterable<Board> neighbors() {     // all neighboring boards
    ArrayList<Board> nn = new ArrayList<>();
    maybeAddBoard(nn, spaceRow, spaceCol, spaceRow - 1, spaceCol);
    maybeAddBoard(nn, spaceRow, spaceCol, spaceRow + 1, spaceCol);
    maybeAddBoard(nn, spaceRow, spaceCol, spaceRow, spaceCol - 1);
    maybeAddBoard(nn, spaceRow, spaceCol, spaceRow, spaceCol + 1);
    return nn;
  }

  private void maybeAddBoard(ArrayList<Board> nn, int r1, int c1, int r2, int c2) {
    if (r2 >= 0 && r2 < dimension() && c2 >= 0 && c2 < dimension()) {
      nn.add(exchange(r1, c1, r2, c2));
    }
  }

  public String toString() {               // string representation of this board (in the output format specified below)
    StringBuilder sb = new StringBuilder();
    sb.append(dimension()).append("\n");
    for (int i = 0; i < dimension(); ++i) {
      for (int j = 0; j < dimension(); ++j) {
        if (j == 0) sb.append(" ");
        else sb.append("  ");
        sb.append(blocks[i][j]);
      }
      sb.append("\n");
    }
    return sb.toString();
  }

  private Board exchange(int r1, int c1, int r2, int c2) {
    int[][] copy = copy(blocks);
    copy[r1][c1] = blocks[r2][c2];
    copy[r2][c2] = blocks[r1][c1];
    return new Board(copy);
  }

  private int[][] copy(int[][] orig) {
    final int N = orig.length;
    int[][] copy = new int[N][N];
    for (int i = 0; i < N; ++i) {
      for (int j = 0; j < N; ++j) {
        copy[i][j] = orig[i][j];
      }
    }
    return copy;
  }


  public static void main(String[] args) { // unit tests (not graded)
    Board b = new Board(new int[][] { {0, 1, 3}, {4, 2, 5}, {7, 8, 6} });
    for (Board n : b.neighbors()) {
      log(n);
    }
  }

  private static void log(Object o) {
    System.out.println(o.toString());
  }
}
