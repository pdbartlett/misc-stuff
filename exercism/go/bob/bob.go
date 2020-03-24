// Package bob is a simple teenager simulator.
package bob

import "strings"

// Hey gets Bob's reaction to a remark.
func Hey(remark string) string {
	trimmed := strings.TrimSpace(remark)
	switch {
	case trimmed == "":
		return "Fine. Be that way!"
	case isYelling(trimmed):
		if isQuestion(trimmed) {
			return "Calm down, I know what I'm doing!"
		}
		return "Whoa, chill out!"
	case isQuestion(trimmed):
		return "Sure."
	default:
		return "Whatever."
	}
}

func isQuestion(s string) bool {
	return strings.HasSuffix(s, "?")
}

func isYelling(s string) bool {
	return s == strings.ToUpper(s) &&
		strings.IndexAny(strings.ToUpper(s), "ABCDEFGHIJKLMNOPQRSTUVWXYZ") >= 0
}
