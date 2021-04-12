package main

import (
  "log"
  "os"
  "path"
)

func main() {
  if len(os.Args) != 3 {
    log.Fatalf("Usage: %s input-file output-file", path.Base(os.Args[0]))
  }
  i, err := os.Open(os.Args[1])
  if err != nil {
    log.Fatalf("Cannot open %q for reading", os.Args[1])
  }
  defer i.Close()
  o, err := os.Create(os.Args[2])
  if err != nil {
    log.Fatalf("Cannot create new file %q", os.Args[2])
  }
  defer o.Close()
}
