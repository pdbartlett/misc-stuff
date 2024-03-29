package main

import (
  "log"

  "github.com/pdbartlett/misc-stuff/golang/signifo"
)

func main() {
  for err := range new(signifo.Server).RunAsync(":2305") {
    log.Printf("ERROR: %v", err)
  }
  log.Println("Exiting")
}
