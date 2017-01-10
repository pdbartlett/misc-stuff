import edu.princeton.cs.algs4.Point2D;
import edu.princeton.cs.algs4.RectHV;
import edu.princeton.cs.algs4.SET;
import edu.princeton.cs.algs4.StdDraw;

public class PointSET {
  private SET<Point2D> points = new SET<>();

  public PointSET() {                           // construct an empty set of points
  }

  public boolean isEmpty() {                    // is the set empty?
    return points.isEmpty();
  }

  public int size() {                           // number of points in the set
    return points.size();
  }

  public void insert(Point2D p) {               // add the point to the set (if it is not already in the set)
    points.add(p);
  }

  public boolean contains(Point2D p) {          // does the set contain point p?
    return points.contains(p);
  }

  public void draw() {                          // draw all points to standard draw
    for (Point2D p : points) {
      StdDraw.point(p.x(), p.y());
    }
    StdDraw.show();
  }

  public Iterable<Point2D> range(RectHV rect) { // all points that are inside the rectangle
    SET<Point2D> inside = new SET<>();
    for (Point2D p : points) {
      if (rect.contains(p)) inside.add(p);
    }
    return inside;
  }

  public Point2D nearest(Point2D p) {           // a nearest neighbor in the set to point p; null if the set is empty
    Point2D bestP = null;
    double bestD = Double.POSITIVE_INFINITY;
    for (Point2D q : points) {
      double d = q.distanceTo(p);
      if (d < bestD) {
        bestD = d;
        bestP = q;
      }
    }
    return bestP;
  }

  public static void main(String[] args) {      // unit testing of the methods (optional)
  }
}
