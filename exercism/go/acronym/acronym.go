// Package acronym generates abbreviated acronyms.
package acronym

import (
	"strings"
)

// Abbreviate returns the acronym for a text string.
func Abbreviate(s string) string {
	var normal strings.Builder
	for _, c := range strings.ToUpper(s) {
		switch {
		case c == '\'':
		case c >= 'A' && c <= 'Z':
			normal.WriteRune(c)
		default:
			normal.WriteRune(' ')
		}
	}
	var acronym strings.Builder
	for _, w := range strings.Split(normal.String(), " ") {
		if len(w) > 0 {
			acronym.WriteByte(w[0])
		}
	}
	return acronym.String()
}
