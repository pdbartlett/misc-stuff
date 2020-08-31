cc_library(
  name = "pipette",
  srcs = ["pipette.h"],
)

cc_binary(
  name = "testapp",
  srcs = ["main.cc"],
  deps = [
    ":pipette",
    "@com_google_absl//absl/memory"
  ],
)
