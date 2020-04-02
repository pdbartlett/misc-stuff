// Package clock implements an hours:minutes clock type.
package clock

import "fmt"

type Clock struct {
  minutes int
}

func New(hour, minute int) Clock {
  c := new(Clock)
  c.minutes = ((60 * hour) + minute) % 1440
  if c.minutes < 0 {
    c.minutes += 1440
  }
  return *c
}

func (c Clock) String() string {
  h := c.minutes / 60
  m := c.minutes % 60
  return fmt.Sprintf("%02d:%02d", h, m)
}

func (c Clock) Add(minutes int) Clock {
  return New(0, c.minutes + minutes)
}

func (c Clock) Subtract(minutes int) Clock {
  return c.Add(-minutes)
}
