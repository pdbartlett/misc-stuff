package main

import (
  "bufio"
  "io"
  "fmt"
  "log"
  "os"
  "path"
  "strings"
)

type ternary int
const (
  YES ternary = iota
  NO
  MAYBE
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
  var buf strings.Builder
  isIndi := MAYBE

  for {
    ch, err := r.Peek(1)
    if err != nil {
      return err
    }
    if isIndi == MAYBE && ch[0] != '0' {
      return fmt.Errorf("expecting %q, got %q", '0', ch[0])
    }
    if isIndi != MAYBE && ch[0] == '0' {
      break
    }

    line, err := r.ReadString('\n')

    if isIndi == MAYBE {
      if strings.HasSuffix(line, "INDI\n") {
        isIndi = YES
      } else {
        isIndi = NO
      }
    }

    if isIndi == NO {
      if _, werr := w.WriteString(line); werr != nil {
        return werr
      }
    } else {
      buf.WriteString(line)
    }

    if err == io.EOF {
      break
    }
    if err != nil {
      return err
    }
  }

  if isIndi == YES {
    br := bufio.NewReader(strings.NewReader(buf.String()))
    for {
      line, berr := br.ReadString('\n')
      switch {
      case strings.HasPrefix(line, "0"), strings.HasPrefix(line, "1 FAM"):
        if _, werr := w.WriteString(line); werr != nil {
          return werr
        }
      default:
        // Do nothing.
      }
      if berr == io.EOF {
        break
      }
      if berr != nil {
        return berr
      }
    }
    if _, werr := w.WriteString("1 NAME Hidden /Person/\n"); werr != nil {
      return werr
    }
  }

  return err
}
