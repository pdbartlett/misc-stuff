package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type state int

const (
	singleton state = iota
)

type lineProcessor interface {
	processLine(line string) (state, error)
}

type sproc struct{}

func (p sproc) processLine(line string) (state, error) {
	fmt.Println(line)
	return singleton, nil
}

var (
	sm = map[state]lineProcessor{singleton: sproc{}}
)

func main() {
	argc := len(os.Args)
	if argc < 2 {
		log.Fatalf("Expected at least 2 arguments, got %d (%v)", argc, os.Args)
	}
	log.Printf("%d arg(s): %v\n", argc, os.Args)

	finPath := os.Args[1]
	fin, err := os.Open(finPath)
	if err != nil {
		log.Fatalf("Error reading file %q: %s", finPath, err.Error())
	}
	defer fin.Close()

	scanner := bufio.NewScanner(fin)
	var st state
	for scanner.Scan() {
		line := scanner.Text()
		st, err := sm[st].processLine(line)
		if err != nil {
			log.Fatalf("Error processing line (state=%v) %q: %s", st, line, err.Error())
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("Error scanning file %q: %s", finPath, err.Error())
	}
}
