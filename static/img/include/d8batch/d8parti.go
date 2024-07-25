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
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"time"
	"unsafe"

	"gopkg.in/yaml.v3"
)

type Step struct {
	Stepname string `yaml:"stepname"`
	Exec     Exec
	Dd       []Dd
}

type Exec struct {
	Pgm string `yaml:"pgm"`
}

type Dd struct {
	Name         string `yaml:"name"`
	Dsn          string `yaml:"dsn"`
	Disp         string `yaml:"disp"`
	Normaldisp   string `yaml:"normaldisp"`
	Abnormaldisp string `yaml:"abnormaldisp"`
}

var step Step

var (
	stepname = flag.String("stepname", "step.yaml", "Name of yaml step exec")
)

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

func config(s string) error {
	configFile, err := os.Open(s)
	if err != nil {
		return err
	}
	log.Println("INFO: Successfully opened config yaml file")
	defer configFile.Close()

	data, _ := io.ReadAll(configFile)

	err = yaml.Unmarshal(data, &step)
	if err != nil {
		return err
	}

	return nil
}

func main() {
	start := time.Now()
	flag.Parse()
	cobInit()

	//Read yaml config file
	err := config(*stepname)
	if err != nil {
		log.Printf("ERROR: Reading config file %s. %s", *stepname, err)
		os.Exit(12)
	}

	//Create Symlink
	for i := 0; i < len(step.Dd); i++ {
		err := os.Symlink(step.Dd[i].Dsn, step.Dd[i].Name)
		if err != nil {
			switch {
			case os.IsExist(err):
				/*DDNAME already exist */
				log.Printf("ERROR: DDNAME=%s already exists. %s", step.Dd[i].Name, err)
			case !os.IsExist(err):
				/*DDNAME invalid */
				log.Printf("ERROR: DDNAME=%s invalid ddname. %s", step.Dd[i].Name, err)
			default:
				log.Println(err)
			}
			os.Exit(04)
		}
	}
	//Call COBOL program -> EXEC PGM defined in JCL
	ret, err := cobCall(step.Exec.Pgm)
	if err != nil {
		fmt.Println(err)
	}

	//Delete Symlink
	for i := 0; i < len(step.Dd); i++ {
		err := os.Remove(step.Dd[i].Name)
		if err != nil {
			log.Printf("INFO: DDNAME=%s does not exists. %s", step.Dd[i].Name, err)

		}
	}

	elapsed := time.Since(start)
	log.Printf("INFO: %s elapsed time %s", step.Exec.Pgm, elapsed)

	cobStop(ret)

}
