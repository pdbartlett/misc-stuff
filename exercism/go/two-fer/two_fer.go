// Package twofer generates text for describing sharing.
package twofer

// ShareWith describes sharing with someone, or "you" by default.
func ShareWith(name string) string {
	if len(name) == 0 {
		name = "you"
	}
	return "One for " + name + ", one for me."
}
