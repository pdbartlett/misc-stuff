package main

import (
  "bufio"
  "io"
  "fmt"
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
  defer w.Flush()

  for {
    err = processBlock(r, w)
    if err == io.EOF {
      log.Println("Successfully reached EOF")
      break
    }
    if err != nil {
      log.Fatalf("Error whilst processing file: %v", err)
    }
  }
}

func processBlock(r *bufio.Reader, w *bufio.Writer) error {
  var err error
  start := true
  for {
    buf, err := r.Peek(1)
    if err != nil {
      return err
    }
    if start && (buf[0] != '0') {
      return fmt.Errorf("expecting %q, got %q", '0', buf[0])
    }
    if (!start) && (buf[0] == '0') {
      break
    }

    line, err := r.ReadString('\n')
    if len(line) > 0 {
      if _, werr := w.WriteString(line); err != nil {
        return werr
      }
    }

    start = false

    if err == io.EOF {
      break
    }
    if err != nil {
      return err
    }
  }
  return err
}
