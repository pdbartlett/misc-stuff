package main

import (
	"bufio"
	"encoding/csv"
	"fmt"
	"os"
	"strings"
)

func main() {
	if err := run(); err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}
}

func run() error {
	type mode int
	const (
		preamble mode = iota
		template
		postamble
	)

	t, err := os.Open(os.Args[1])
	if err != nil {
		return err
	}
	defer t.Close()

	tr := bufio.NewReader(t)
	var tmpl, post string
	m := preamble
	for {
		line, err := tr.ReadString('\n')
		if line == "@@@\n" {
			m++
			continue
		}
		switch m {
		case preamble:
			fmt.Print(line)
		case template:
			tmpl += line
		case postamble:
			post += line
		}
		if err != nil {
			break
		}
	}

	f, err := os.Open(os.Args[2])
	if err != nil {
		return err
	}
	defer f.Close()

	r := csv.NewReader(f)
	data, err := r.ReadAll()
	if err != nil {
		return nil
	}

	for _, row := range data {
		out := tmpl
		for i, s := range row {
			out = strings.Replace(out, fmt.Sprintf("$%d", i+1), s, -1)
		}
		fmt.Print(out)
	}
	fmt.Print(post)

	return nil
}
