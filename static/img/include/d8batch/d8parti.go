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

func config() error {
	// Read yaml config file
	viper.SetConfigName("step")
	viper.SetConfigType("yaml")
	viper.AddConfigPath(".")
	if err := viper.ReadInConfig(); err != nil {
		return err
	}
	// Unmarshal yaml config file
	if err := viper.Unmarshal(&Step); err != nil {
		return err
	}
	// Create Symlink
	for i := 0; i < len(Step.Dd); i++ {
		err := os.Symlink(Step.Dd[i].Dsn, Step.Dd[i].Name)
		if err != nil {
			switch {
			case os.IsExist(err):
				// DDNAME already exist
				log.Printf("INFO: DDNAME=%s already exists. %s", Step.Dd[i].Name, err)
			case os.IsNotExist(err):
				// DDNAME invalid
				log.Printf("ERROR: DDNAME=%s invalid ddname. %s", Step.Dd[i].Name, err)
				return err
			default:
				log.Println(err)
				return err
			}
		}
	}
	return nil
}

func cobCall(p string) error {
	defer delSymlink()
	c_progName := C.CString(p)
	defer C.free(unsafe.Pointer(c_progName))

	n := C.cob_resolve(c_progName)
	if n == nil {
		return fmt.Errorf("ERROR: Program %s not found", p)
	} else {
		log.Printf("INFO: PGM=%s started", p)
		r := C.cob_call_with_exception_check(c_progName, C.int(0), nil)
		rc := int(C.cob_last_exit_code())
		err := C.GoString(C.cob_last_runtime_error())
		switch int(r) {
		case 0:
			log.Printf("INFO: program %s exited with return-code: %v", p, rc)
			C.cob_tidy()
		case 1:
			log.Printf("INFO: program %s STOP RUN with return-code: %v", p, rc)
		case -1:
			return fmt.Errorf("ERROR: program %s exit with return-code: %v and error: %s", p, rc, err)
		case -2:
			return fmt.Errorf("FATAL: program %s exit with return-code: %v and error: %s", p, rc, err)
		case -3:
			return fmt.Errorf("ERROR: program %s signal handler exit with signal: %v and error: %s", p, rc, err)
		default:
			return fmt.Errorf("ERROR: program %s unexpected return exit code: %v and error: %s", p, rc, err)
		}
		return nil
	}
}

func delSymlink() {
	for i := 0; i < len(Step.Dd); i++ {
		err := os.Remove(Step.Dd[i].Name)
		if err != nil {
			log.Printf("INFO: DDNAME=%s does not exists. %s", Step.Dd[i].Name, err)
		}
	}
}

func main() {
	start := time.Now()

	// Initialize gnucobol
	C.cob_init(C.int(0), nil)
	log.Println("INFO: gnucobol initialized")

	// Load config file
	if err := config(); err != nil {
		log.Printf("ERROR: reading yaml config file. %s", err)
		os.Exit(12)
	}

	// Call COBOL program -> EXEC PGM defined in JCL
	if err := cobCall(Step.Exec.Pgm); err != nil {
		log.Println(err)
		os.Exit(12)
	}

	elapsed := time.Since(start)
	log.Printf("INFO: %s elapsed time %s", Step.Exec.Pgm, elapsed)

}
