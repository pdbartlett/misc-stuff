// Package scrabble implements the rules of the word game, Scrabble.
package scrabble

import "strings"

// Score returns the Scrabble score for a word.
func Score(word string) int {
  score := 0
  for _, ch := range strings.ToUpper(word) {
    switch ch {
      case 'A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T':
        score += 1
      case 'D', 'G':
        score += 2
      case 'B', 'C', 'M', 'P':
        score += 3
      case 'F', 'H', 'V', 'W', 'Y':
        score += 4
      case 'K':
        score += 5
      case 'J', 'X':
        score += 8
      case 'Q', 'Z':
        score += 10
    }
  }
  return score
}
