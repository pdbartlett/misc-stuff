package main

import (
  "log"
  "os"
  "path"
)

func main() {
  if len(os.Args) != 2 {
    this, _ := os.Executable()
    log.Fatalf("Usage: %s input-file output-file", path.Base(this))
  }
}
