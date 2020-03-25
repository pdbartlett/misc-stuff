// Package hamming calculates Hamming distance.
package hamming

import (
  "fmt"
)

// Distance calculates the Hamming distance between two strings of equal length.
func Distance(a, b string) (int, error) {
  if len(a) != len(b) {
    return -1, fmt.Errorf("Mismatched lengths: %v and %v", len(a), len(b))
  }
  dist := 0
  for i := 0; i < len(a); i++ {
    if a[i] != b[i] {
      dist += 1
    }
  }
  return dist, nil
}
