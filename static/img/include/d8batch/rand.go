package main

import (
	"flag"
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"time"
)

var r1 *rand.Rand

var (
	rows = flag.Int("rows", 1000, "number of rows to generate")
)

var (
	file = flag.String("file", "test.txt", "input file name")
)

func main() {
	flag.Parse()
	s1 := rand.NewSource(time.Now().UnixNano())
	r1 = rand.New(s1)

	f, err := os.Create(*file)
	if err != nil {
		fmt.Println(err)
		return
	}
	for i := 0; i != *rows; i++ {

		output := account(i) + "-" + amount() + "-" + interest() + "-" + yearsPending() + "\n"
		_, err := f.WriteString(output)
		if err != nil {
			fmt.Println(err)
			f.Close()
			return
		}
	}
	err = f.Close()
	if err != nil {
		fmt.Println(err)
		return
	}
}
func account(id int) string {

	return "id:" + fmt.Sprintf("%07d", id+1)

}

func amount() string {
	min := 1000
	max := 1000000
	a := strconv.Itoa(r1.Intn(max-min+1) + min)
	for i := len(a); i != 7; i++ {
		a = "0" + a
	}
	return a

}

func interest() string {

	return "0450"
}

func yearsPending() string {
	min := 5
	max := 25
	y := strconv.Itoa(r1.Intn(max-min+1) + min)
	if len(y) < 2 {
		y = "0" + y
	}
	return y

}
