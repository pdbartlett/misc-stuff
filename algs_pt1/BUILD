java_binary(
  name = "a0",
  srcs = ["helloworld.java"],
  main_class = "HelloWorld",
)

java_library(
  name = "a1",
  srcs = ["Percolation.java"],
  deps = ["@algs4_jar//jar"],
)

java_binary(
  name = "a1_test",
  runtime_deps = [":a1"],
  main_class = "Percolation",
)

java_binary(
  name = "a1_stats",
  srcs = ["PercolationStats.java"],
  deps = [
    ":a1",
    "@algs4_jar//jar",
  ],
  main_class = "PercolationStats",
)

java_binary(
  name = "a2_deque",
  srcs = ["Deque.java"],
  deps = ["@algs4_jar//jar"],
  main_class = "Deque",
)

java_library(
  name = "a2_randq",
  srcs = ["RandomizedQueue.java"],
  deps = ["@algs4_jar//jar"],
)

java_binary(
  name = "a2_randq_test",
  runtime_deps = [":a2_randq"],
  main_class = "RandomizedQueue",
)

java_binary(
  name = "a2_subset",
  srcs = ["Subset.java"],
  deps = [
    ":a2_randq",
    "@algs4_jar//jar",
  ],
  main_class = "Subset",
)

java_library(
  name = "a3",
  srcs = [
    "BruteCollinearPoints.java",
    "FastCollinearPoints.java",
    "LineSegment.java",
    "Point.java",
  ],
  deps = ["@algs4_jar//jar"],
)

java_binary(
  name = "a3_test",
  srcs = ["Fcpt.java"],
  deps = [
    ":a3",
    "@algs4_jar//jar",
  ],
  main_class = "Fcpt",
)

java_library(
  name = "a4_board",
  srcs = ["Board.java"],
)

java_binary(
  name = "a4_board_test",
  runtime_deps = [":a4_board"],
  main_class = "Board",
)

java_binary(
  name = "a4_solver",
  srcs = ["Solver.java"],
  deps = [
    ":a4_board",
    "@algs4_jar//jar",
  ],
  main_class = "Solver",
)

java_binary(
  name = "a5_set",
  srcs = ["PointSET.java"],
  deps = ["@algs4_jar//jar"],
  main_class = "PointSET",
)


java_library(
  name = "a5_kdtree",
  srcs = ["KdTree.java"],
  deps = ["@algs4_jar//jar"],
)

java_binary(
  name = "a5_test",
  runtime_deps = [":a5_kdtree"],
  main_class = "KdTree",
)

java_binary(
  name = "a5_viz",
  srcs = ["KdTreeVisualizer.java"],
  deps = [
    ":a5_kdtree",
    "@algs4_jar//jar",
  ],
  main_class = "KdTreeVisualizer",
)
