// Package diffsquares calculates sum of squares, square of sum, and difference.
package diffsquares

// SumOfSquares calculates the sum of the squares from 1..n.
func SumOfSquares(n int) int {
  ss := 0
  for i := 1; i <= n; i++ {
    ss += (i*i)
  }
  return ss
}

// SquareOfSum calculates the square of the sum from 1..n.
func SquareOfSum(n int) int {
  s := (n * (n+1)) / 2
  return s*s
}

// Difference calculates the difference between sum of squares and square of sum.
func Difference(n int) int {
  return SquareOfSum(n) - SumOfSquares(n)
}
