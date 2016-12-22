import edu.princeton.cs.algs4.Point2D;
import edu.princeton.cs.algs4.RectHV;
import edu.princeton.cs.algs4.SET;
import edu.princeton.cs.algs4.StdDraw;

public class KdTree {
  private enum Direction {
    VERT,
    HORZ;

    Direction opp() {
      return this == Direction.VERT ? Direction.HORZ : Direction.VERT;
    }
  }

  private static class Node {
    private Point2D p;
    private RectHV r;
    private Direction dir;
    private Node lb;
    private Node rt;
  }

  private Node root;
  private int size;

  public KdTree() {                             // construct an empty set of points
  }

  public boolean isEmpty() {                    // is the set empty?
    return root == null;
  }

  public int size() {                           // number of points in the set
    return size;
  }

  public void insert(Point2D p) {               // add the point to the tree (if it is not already in the tree)
    root = insert(root, null, p, false);
  }

  private Node insert(Node s, Node prev, Point2D p, boolean isLb) {
    if (s == null) {
      Node n = new Node();
      n.p = p;
      if (prev == null) {
        n.dir = Direction.VERT;
        n.r = new RectHV(0, 0, 1, 1);
      } else {
        n.dir = prev.dir.opp();
        if (n.dir == Direction.VERT) {
          n.r = new RectHV(prev.r.xmin(), isLb ? prev.r.ymin() : prev.p.y(),
                           prev.r.xmax(), isLb ? prev.p.y() : prev.r.ymax());
        } else {
          n.r = new RectHV(isLb ? prev.r.xmin() : prev.p.x(), prev.r.ymin(),
                           isLb ? prev.p.x() : prev.r.xmax(), prev.r.ymax());
        }
      }
      ++size;
      return n;
    }
    if (s.p.equals(p)) return s;
    double sv = s.dir == Direction.VERT ? s.p.x() : s.p.y();
    double pv = s.dir == Direction.VERT ? p.x() : p.y();
    if (pv < sv) s.lb = insert(s.lb, s, p, true);
    else s.rt = insert(s.rt, s, p, false);
    return s;
  }

  public boolean contains(Point2D p) {          // does the set contain point p?
    return contains(root, p);
  }

  private boolean contains(Node n, Point2D p) {
    if (n == null) return false;
    if (n.p.equals(p)) return true;
    double nv = n.dir == Direction.VERT ? n.p.x() : n.p.y();
    double pv = n.dir == Direction.VERT ? p.x() : p.y();
    return contains(pv < nv ? n.lb : n.rt, p);
  }

  public void draw() {                          // draw all points to standard draw
    draw(root);
    StdDraw.show();
  }

  private void draw(Node n) {
    if (n == null) return;
    StdDraw.setPenColor(StdDraw.BLACK);
    StdDraw.setPenRadius(0.01);
    StdDraw.point(n.p.x(), n.p.y());
    StdDraw.setPenRadius();
    if (n.dir == Direction.VERT) {
      StdDraw.setPenColor(StdDraw.RED);
      StdDraw.line(n.p.x(), n.r.ymin(), n.p.x(), n.r.ymax());
    } else {
      StdDraw.setPenColor(StdDraw.BLUE);
      StdDraw.line(n.r.xmin(), n.p.y(), n.r.xmax(), n.p.y());
    }
    draw(n.lb);
    draw(n.rt);
  }

  public Iterable<Point2D> range(RectHV rect) { // all points that are inside the rectangle
    SET<Point2D> inside = new SET<>();
    range(root, rect, inside);
    return inside;
  }

  private void range(Node n, RectHV rect, SET<Point2D> inside) {
    if (n == null || !n.r.intersects(rect)) return;
    if (rect.contains(n.p)) inside.add(n.p);
    range(n.lb, rect, inside);
    range(n.rt, rect, inside);
  }

  public Point2D nearest(Point2D p) {           // a nearest neighbor in the tree to point p; null if the tree is empty
    return nearest(root, p, null);
  }

  private Point2D nearest(Node n, Point2D p, Point2D bestP) {
    if (n == null) return bestP;
    double bestD = bestP == null ? Double.POSITIVE_INFINITY : p.distanceTo(bestP);
    if (bestP != null) {
      double rnx = p.x() < n.r.xmin() ? n.r.xmin() : (p.x() > n.r.xmax() ? n.r.xmax() : p.x());
      double rny = p.y() < n.r.ymin() ? n.r.ymin() : (p.y() > n.r.ymax() ? n.r.ymax() : p.y());
      if (p.distanceTo(new Point2D(rnx, rny)) >= bestD) return bestP;
    }

    if (n.p.distanceTo(p) < bestD) bestP = n.p;

    double nv = n.dir == Direction.VERT ? n.p.x() : n.p.y();
    double pv = n.dir == Direction.VERT ? p.x() : p.y();
    if (pv < nv) {
      bestP = nearest(n.lb, p, bestP);
      bestP = nearest(n.rt, p, bestP);
    } else {
      bestP = nearest(n.rt, p, bestP);
      bestP = nearest(n.lb, p, bestP);
    }

    return bestP;
  }

  public static void main(String[] args) {      // unit testing of the methods (optional)
    KdTree t = new KdTree();
    t.insert(new Point2D(0.6, 0.4));
    t.insert(new Point2D(0.8, 0.1));
    t.draw();
  }
}
