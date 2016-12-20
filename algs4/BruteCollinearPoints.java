import java.util.ArrayList;
import java.util.Arrays;

public class BruteCollinearPoints {

  private ArrayList<LineSegment> results = new ArrayList<>();

  public BruteCollinearPoints(Point[] points) {    // finds all line segments containing 4 points
    final int N = points.length;
    Point[] sorted = Arrays.copyOf(points, N);
    Arrays.sort(sorted);
    for (int ix = 1; ix < N; ++ix) {
      if (sorted[ix].compareTo(sorted[ix - 1]) == 0) {
        throw new IllegalArgumentException("Repeated value: " + sorted[ix]);
      }
    }
    for (int i = 0; i < N; ++i) {
      for (int j = i + 1; j < N; ++j) {
        double slope_ij = sorted[i].slopeTo(sorted[j]);
        for (int k = j + 1; k < N; ++k) {
          double slope_ik = sorted[i].slopeTo(sorted[k]);
          if (slope_ik != slope_ij) continue;
          for (int l = k + 1; l < N; ++l) {
            double slope_il = sorted[i].slopeTo(sorted[l]);
            if (slope_il == slope_ij) results.add(new LineSegment(sorted[i], sorted[l]));
          }
        }
      }
    }
  }

  public int numberOfSegments() {                  // the number of line segments
    return results.size();
  }

  public LineSegment[] segments() {                // the line segments
    return results.toArray(new LineSegment[0]);
  }
}
