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
