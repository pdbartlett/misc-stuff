// Package luhn implements Luhn checking.
package luhn

import (
  "strings"
)

// Valid returns whether a credit card number is valid.
func Valid(cc string) bool {
  var norm strings.Builder
  for _, r := range cc {
    switch {
    case r == ' ':
      continue
    case '0' <= r && r <= '9':
      norm.WriteRune(r)
    default:
      return false
    }
  }
  s := norm.String()
  n := len(s)
  if n == 1 {
    return false
  }
  var a byte
  for i := 1; i <= n; i++ {
    d := s[n-i] - '0'
    if i % 2 == 0 {
      d *= 2
      if d > 9 {
        d -= 9
      }
    }
    a += d
  }
  return (a % 10) == 0
}
