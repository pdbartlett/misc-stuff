import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.MinPQ;
import edu.princeton.cs.algs4.StdOut;

import java.util.Arrays;

public class Solver {
  private class SearchNode implements Comparable<SearchNode> {
    private Board board;
    private SearchNode previous;
    private int numMoves;
    private SearchNode(Board board) {
      this.board = board;
    }
    private SearchNode(Board board, SearchNode previous) {
      this.board = board;
      this.previous = previous;
      numMoves = previous.numMoves + 1;
    }
    private int score() { return board.manhattan() + numMoves; }
    public int compareTo(SearchNode that) {
      return score() - that.score();
    }
  }

  private class Worker {
    private final MinPQ<SearchNode> pq = new MinPQ<>();
    private SearchNode solution;

    Worker(Board initial) {
      pq.insert(new SearchNode(initial));
    }

    boolean next() {
      SearchNode top = pq.delMin();
      if (top.board.isGoal()) {
        solution = top;
        return true;
      }
      for (Board n : top.board.neighbors()) {
        if (top.previous == null || !n.equals(top.previous.board)) pq.insert(new SearchNode(n, top));
      }
      return false;
    }
  }

  private SearchNode solution;

  public Solver(Board initial) {           // find a solution to the initial board (using the A* algorithm)
    Worker main = new Worker(initial);
    Worker twin = new Worker(initial.twin());
    while (!main.next() && !twin.next()) { }
    solution = main.solution;
  }

  public boolean isSolvable() {            // is the initial board solvable?
    return solution != null;
  }

  public int moves() {                     // min number of moves to solve initial board; -1 if unsolvable
    return isSolvable() ? solution.numMoves : -1;
  }

  public Iterable<Board> solution() {      // sequence of boards in a shortest solution; null if unsolvable
    if (!isSolvable()) return null;
    Board[] boards = new Board[moves() + 1];
    SearchNode s = solution;
    for (int i = moves(); i >= 0; --i) {
      boards[i] = s.board;
      s = s.previous;
    }
    return Arrays.asList(boards);
  }

  public static void main(String[] args) { // solve a slider puzzle (given below)
    // create initial board from file
    In in = new In(args[0]);
    int n = in.readInt();
    int[][] blocks = new int[n][n];
    for (int i = 0; i < n; i++)
      for (int j = 0; j < n; j++)
        blocks[i][j] = in.readInt();
    Board initial = new Board(blocks);

    // solve the puzzle
    Solver solver = new Solver(initial);

    // print solution to standard output
    if (!solver.isSolvable())
      StdOut.println("No solution possible");
    else {
      StdOut.println("Minimum number of moves = " + solver.moves());
      for (Board board : solver.solution())
        StdOut.println(board);
    }
  }
}
