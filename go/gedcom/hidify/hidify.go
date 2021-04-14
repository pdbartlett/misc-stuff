package main

import (
  "bufio"
  "io"
  "log"
  "os"
  "path"
  "strings"
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

  var b strings.Builder
  for {
    line, err := r.ReadString('\n')
    if len(line) > 0 {
      process(line, w, &b)
    }
    if err == io.EOF {
      break
    }
    if err != nil {
      log.Fatalf("Error whilst reading file: %v", err)
    }
  }
}

func process(line string, w *bufio.Writer, b *strings.Builder) {
  if _, err := w.WriteString(line); err != nil {
    log.Fatalf("Error writing to file: %v", err)
  }
}
