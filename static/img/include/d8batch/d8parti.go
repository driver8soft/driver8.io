package main

/*
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libcob.h>
#cgo CFLAGS: -I/opt/homebrew/Cellar/gnucobol/3.2/include
#cgo LDFLAGS: -L/opt/homebrew/Cellar/gnucobol/3.2/lib -lcob
*/
import "C"
import (
	"errors"
	"fmt"
	"log"
	"os"
	"time"
	"unsafe"

	"github.com/spf13/viper"
)

type step struct {
	Stepname string `mapstructure:"stepname"`
	Exec     exec
	Dd       []dd
}
type exec struct {
	Pgm string `mapstructure:"pgm"`
}
type dd struct {
	Name         string `mapstructure:"name"`
	Dsn          string `mapstructure:"dsn"`
	Disp         string `mapstructure:"disp"`
	Normaldisp   string `mapstructure:"normaldisp"`
	Abnormaldisp string `mapstructure:"abnormaldisp"`
}

var Step *step

func cobInit() {
	C.cob_init(C.int(0), nil)
	log.Println("INFO: gnucobol initialized")
}
func cobCall(p string) (ret int, err error) {
	c_progName := C.CString(p)
	defer C.free(unsafe.Pointer(c_progName))

	n := C.cob_resolve(c_progName)
	if n == nil {
		return 12, errors.New("ERROR: Module not found")
	} else {
		log.Printf("INFO: PGM=%s started", p)
		r := C.cob_call(c_progName, C.int(0), nil)
		log.Printf("INFO: %s return-code %v", p, ret)
		return int(r), nil

	}
}
func cobStop(ret int) {
	C.cob_stop_run(C.int(ret))
}
func main() {
	start := time.Now()

	cobInit()

	// Read yaml config file
	viper.SetConfigName("step")
	viper.SetConfigType("yaml")
	viper.AddConfigPath(".")
	if err := viper.ReadInConfig(); err != nil {
		fmt.Println(err)
	}
	// Unmarshal yaml config file
	if err := viper.Unmarshal(&Step); err != nil {
		fmt.Println(err)
	}
	// Create Symlink
	for i := 0; i < len(Step.Dd); i++ {
		err := os.Symlink(Step.Dd[i].Dsn, Step.Dd[i].Name)
		if err != nil {
			switch {
			case os.IsExist(err):
				// DDNAME already exist
				log.Printf("INFO: DDNAME=%s already exists. %s", Step.Dd[i].Name, err)
			case !os.IsExist(err):
				// DDNAME invalid
				log.Printf("ERROR: DDNAME=%s invalid ddname. %s", Step.Dd[i].Name, err)
				os.Exit(04)
			default:
				log.Println(err)
			}
		}
	}
	// Call COBOL program -> EXEC PGM defined in JCL
	ret, err := cobCall(Step.Exec.Pgm)
	if err != nil {
		fmt.Println(err)
	}
	// Delete Symlink
	for i := 0; i < len(Step.Dd); i++ {
		err := os.Remove(Step.Dd[i].Name)
		if err != nil {
			log.Printf("INFO: DDNAME=%s does not exists. %s", Step.Dd[i].Name, err)
		}
	}
	elapsed := time.Since(start)
	log.Printf("INFO: %s elapsed time %s", Step.Exec.Pgm, elapsed)

	cobStop(ret)
}
