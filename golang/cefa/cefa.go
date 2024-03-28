package main

import (
  "log"

  "github.com/pdbartlett/misc-stuff/golang/signifo"
)

func main() {
  if err := new(signifo.Server).Run(); err != nil {
    log.Fatalf("Server.Run() failed with %v", err)
  }
  log.Println("Exiting normally")
}
