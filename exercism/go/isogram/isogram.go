// Package isogram tests for isograms.
package isogram

import "strings"

// IsIsogram returns whether a word is an isogram.
func IsIsogram(word string) bool {
  mem := map[rune]bool{}
  for _, ch := range strings.ToUpper(word) {
    if ch >= 'A' && ch <= 'Z' {
      if _, ok := mem[ch]; ok {
        return false
      }
      mem[ch] = true
    }
  }
  return true
}
