package main

import (
	"bufio"
	"bytes"
	"encoding/csv"
	"fmt"
	"log"
	"os"
	"strings"
	"text/template"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	t, err := os.Open(os.Args[1])
	if err != nil {
		return err
	}
	defer t.Close()

	tr := bufio.NewReader(t)
	var blocks []string
	var buffer strings.Builder
	for {
		line, err := tr.ReadString('\n')
		if line == "@@@\n" {
			blocks = append(blocks, buffer.String())
			buffer.Reset()
			continue
		}
		if err != nil {
			break
		}
		buffer.WriteString(line)
	}
	blocks = append(blocks, buffer.String())
	log.Print(blocks)

	numCsvs := len(os.Args) - 3
	if len(blocks) - 2 !=  numCsvs{
		return fmt.Errorf("Got %d text blocks so expected %d CSV files (got %d)",
		    len(blocks), len(blocks)-2, numCsvs)
	}

	fmt.Print(blocks[0])

	for i := 0; i < numCsvs; i++ {
		tmpl, err := template.New(fmt.Sprintf("template%d", i)).Parse(blocks[i+1])
		if err != nil {
			return err
		}

		f, err := os.Open(os.Args[i+2])
		if err != nil {
			return err
		}
		defer f.Close()

		r := csv.NewReader(f)
		data, err := r.ReadAll()
		if err != nil {
			return err
		}

		for _, row := range data {
			var out bytes.Buffer
			err := tmpl.Execute(&out, row)
			if err != nil {
				return err
			}
			fmt.Print(out.String())
		}
	}

	fmt.Print(blocks[len(blocks)-1])

	return nil
}
