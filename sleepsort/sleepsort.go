package sleepsort

import (
	"time"
)

func sleepsort(input []uint32) []uint32 {
	done := make(chan uint32, len(input))
	for _, in := range input {
		go func(v uint32) {
			time.Sleep(time.Duration(v) * (time.Millisecond / 100))
			done <- v
		}(in)
	}

	out := make([]uint32, len(input))
	for i := 0; i < len(input); i++ {
		out[i] = <-done
	}
	return out
}
