// Package gigasecond contains a method to add a gigasecond to a time (exercism).
package gigasecond

import "time"

// AddGigasecond returns a time with a gigasecond added.
func AddGigasecond(t time.Time) time.Time {
	return t.Add(time.Second * 1e9)
}
