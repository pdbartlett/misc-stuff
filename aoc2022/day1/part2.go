package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "sort"
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
    currv := 0
    stg := []int{}
    for scanner.Scan() {
        line := strings.TrimSpace(scanner.Text())
        if line == "" {
            stg = append(stg, currv)
            currv = 0
            continue
        }
        i, err := strconv.Atoi(line)
        if err != nil {
            log.Fatalf("Parsing %q: %v", line, err)
        }
        currv += i
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
    
    sort.Ints(stg)
    c := len(stg)
    fmt.Printf("Answer = %d\n", stg[c-1]+stg[c-2]+stg[c-3])
}