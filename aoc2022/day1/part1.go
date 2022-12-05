package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "strconv"
    "strings"
)

func main() {
    file, err := os.Open(os.Args[1])
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    besti := -1
    bestv := -1
    curri := 0
    currv := 0
    for scanner.Scan() {
        line := strings.TrimSpace(scanner.Text())
        if line == "" {
            curri += 1
            currv = 0
            continue
        }
        i, err := strconv.Atoi(line)
        if err != nil {
            log.Fatalf("Parsing %q: %v", line, err)
        }
        currv += i
        if currv > bestv {
            bestv = currv
            besti = curri
        }
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
    
    fmt.Printf("Best elf is #%d with %d calories\n", besti, bestv)
}