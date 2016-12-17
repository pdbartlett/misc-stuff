java_binary(
  name = "assignment0",
  srcs = ["helloworld.java"],
  main_class = "HelloWorld",
)

java_library(
  name = "assignment1",
  srcs = ["Percolation.java"],
  deps = ["@algs4_jar//jar"],
)

java_binary(
  name = "assignment1_test",
  runtime_deps = [":assignment1"],
  main_class = "Percolation",
)

java_binary(
  name = "assignment1_stats",
  srcs = ["PercolationStats.java"],
  deps = [
    ":assignment1",
    "@algs4_jar//jar",
  ],
  main_class = "PercolationStats",
)
