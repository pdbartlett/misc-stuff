import java.util.ArrayList;
import java.util.Arrays;

public class FastCollinearPoints {
  private ArrayList<LineSegment> results = new ArrayList<>();

  public FastCollinearPoints(Point[] points) {    // finds all line segments containing 4 points
    final int N = points.length;
    if (N < 4) {
      for (int i1 = 0; i1 < N; ++i1) {
        for (int i2 = i1 + 1; i2 < N; ++i2) {
          checkDup(points[i1], points[i2]);
        }
      }
      return;
    }
    Point[] sorted = Arrays.copyOf(points, N);
    for (int i = 0; i < N; ++i) {
      Point p = points[i];
      Arrays.sort(sorted, p.slopeOrder());
      if (p.compareTo(sorted[0]) != 0) throw new AssertionError("Unexpected first value");
      Point end = checkDup(sorted[1], p);
      int count = 1;
      boolean duplicate = p.compareTo(end) > 0;;
      for (int j = 2; j < sorted.length; ++j) {
        Point q = checkDup(sorted[j], p);
        if (p.slopeTo(q) == p.slopeTo(end)) {
          ++count;
          if (q.compareTo(end) > 0) end = q;
          if (p.compareTo(q) > 0) duplicate = true;
        } else {
          if (count >= 3 && !duplicate) {
            results.add(new LineSegment(p, end));
          }
          count = 1;
          end = q;
          duplicate = p.compareTo(q) > 0;
        }
      }
      if (count >= 3 && !duplicate) {
        results.add(new LineSegment(p, end));
      }
    }
  }

  public int numberOfSegments() {                 // the number of line segments
    return results.size();
  }

  public LineSegment[] segments() {               // the line segments
    return results.toArray(new LineSegment[0]);
  }

  private Point checkDup(Point p, Point ref) {
    if (p.compareTo(ref) == 0) throw new IllegalArgumentException("Repeated value: " + p);
    return p;
  }
}
