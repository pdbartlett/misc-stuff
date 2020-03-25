// Package space models the solar system planets.
package space

// A planet of the solar system.
type Planet string

const (
  Mercury Planet = "Mercury"
  Venus          = "Venus"
  Earth          = "Earth"
  Mars           = "Mars"
  Jupiter        = "Jupiter"
  Saturn         = "Saturn"
  Uranus         = "Uranus"
  Neptune        = "Neptune"
)

// Age converts a number of seconds to years (solar orbital periods) for a given planet.
func Age(seconds float64, planet Planet) float64 {
  relYears := map[Planet]float64{
    Mercury : 0.2408467,
    Venus   : 0.61519726,
    Earth   : 1.0,
    Mars    : 1.8808158,
    Jupiter : 11.862615,
    Saturn  : 29.447498,
    Uranus  : 84.016846,
    Neptune : 164.79132,
  }
  return (seconds / 31557600) / relYears[planet]
}
