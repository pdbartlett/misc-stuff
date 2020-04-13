package letter

// FreqMap records the frequency of each rune in a given text.
type FreqMap map[rune]int

// Frequency counts the frequency of each rune in a given text and returns this
// data as a FreqMap.
func Frequency(s string) FreqMap {
	m := FreqMap{}
	for _, r := range s {
		m[r]++
	}
	return m
}

// AdaptFrequency uses the passed channel to adapt Frequency for concurrent use.
func AdaptFrequency(s string, ch chan FreqMap) {
	ch <- Frequency(s)
}

// ConcurrentFrequency concurrently computes rune frequency using Frequency.
func ConcurrentFrequency(ss []string) FreqMap {
	ch := make(chan FreqMap)
	for _, s := range ss {
		go AdaptFrequency(s, ch)
	}
	totals := make(FreqMap)
	for i := 0; i < len(ss); i++ {
		fm := <-ch
		for k, v := range fm {
			totals[k] = totals[k] + v
		}
	}
	return totals
}
