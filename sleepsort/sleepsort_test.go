package sleepsort

import (
	"log"
	"testing"
)

func TestMain(t *testing.T) {
	v := sleepsort([]uint32{7, 3, 5, 10, 11, 20, 12, 50, 1024, 888})
	log.Printf("%v", v)
}
