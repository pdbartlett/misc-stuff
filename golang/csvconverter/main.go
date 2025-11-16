package main

import (
	//"bufio"
	//"encoding/csv"
	"fmt"
	"os"
	//"strings"
)

func main() {
	if err := run(); err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}
}

func run() error {
    return nil
}
