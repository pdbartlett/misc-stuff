package main

import (
  "bufio"
  "io"
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
    log.Fatalf("Cannot open %q for reading: %v", os.Args[1], err)
  }
  defer i.Close()
  r := bufio.NewReader(i)

  o, err := os.Create(os.Args[2])
  if err != nil {
    log.Fatalf("Cannot create new file %q: %v", os.Args[2], err)
  }
  defer o.Close()
  w := bufio.NewWriter(o)

  for {
    line, err := r.ReadString('\n')
    if len(line) > 0 {
      if _, werr := w.WriteString(line); werr != nil {
        log.Fatalf("Error writing to file: %v", werr)
      }
    }
    if err == io.EOF {
      break
    }
    if err != nil {
      log.Fatalf("Error whilst reading file: %v", err)
    }
  }
}
