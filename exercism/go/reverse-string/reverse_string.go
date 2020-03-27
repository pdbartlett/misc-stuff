// Package reverse allows strings to be reversed.
package reverse

// Reverse returns the reversed version of the specified string.
func Reverse(s string) string {
  n := len(s)
  b := make([]rune, n)
  for _, r := range s {
      n--
      b[n] = r
  }
  return string(b[n:])
}
