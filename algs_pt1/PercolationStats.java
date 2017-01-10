import edu.princeton.cs.algs4.StdOut;
import edu.princeton.cs.algs4.StdRandom;
import edu.princeton.cs.algs4.StdStats;

public class PercolationStats {
  private final double[] data;
  public PercolationStats(int n, int trials) {    // perform trials independent experiments on an n-by-n grid
    if (n <= 0 || trials <= 0) throw new IllegalArgumentException();
    data = new double[trials];
    for (int t = 0; t < trials; ++t) {
      Percolation p = new Percolation(n);
      int count = 0;
      while (!p.percolates()) {
        count++;
        while (true) {
          int r = StdRandom.uniform(n) + 1;
          int c = StdRandom.uniform(n) + 1;
          if (!p.isOpen(r, c)) {
            p.open(r, c);
            break;
          }
        }
      }
      data[t] = ((double) count) / (n * n);
    }
  }

  public double mean() {                          // sample mean of percolation threshold
    return StdStats.mean(data);
  }

  public double stddev() {                        // sample standard deviation of percolation threshold
    return StdStats.stddev(data);
  }

  public double confidenceLo() {                  // low endpoint of 95% confidence interval
    return mean() - halfConfidenceRange();
  }

  public double confidenceHi() {                  // high endpoint of 95% confidence interval
    return mean() + halfConfidenceRange();
  }

  private double halfConfidenceRange() {
    return 1.96 * stddev() / Math.sqrt(data.length);
  }

  public static void main(String[] args) {        // test client
    PercolationStats ps = new PercolationStats(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
    StdOut.printf("mean: %f\n", ps.mean());
    StdOut.printf("stddev: %f\n", ps.stddev());
    StdOut.printf("95%% confidence interval: %f, %f\n", ps.confidenceLo(), ps.confidenceHi());
  }
}
