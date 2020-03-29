// Package grains calculates number of grains on chessboard
package grains

import "fmt"

// Square returns the number of grains on a given square (1-64) or an error otherwise.
func Square(n int) (uint64, error) {
  if n < 1 || n > 64 {
    return 0, fmt.Errorf("%v is not a valid square number (1-64)", n)
  }
  return 1 << (n-1), nil
}

// Total returns the total number of grains on a standard chessboard.
func Total() uint64 {
  var n uint64 = 0
  for i := 1; i <= 64; i++ {
    if v, err := Square(i); err == nil {
      n += v
    }
  }
  return n
}
