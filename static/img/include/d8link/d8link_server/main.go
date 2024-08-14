package main

/*
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libcob.h>
#cgo CFLAGS: -I/opt/homebrew/Cellar/gnucobol/3.2/include
#cgo LDFLAGS: -L/opt/homebrew/Cellar/gnucobol/3.2/lib -lcob
static void* allocArgv(int argc) {
    return malloc(sizeof(char *) * argc);
}
*/
import "C"
import (
	"context"
	"flag"
	"fmt"
	"log"
	"net"
	"strings"
	"time"
	"unsafe"

	pb "github.com/driver8soft/examples/d8link/link"

	"google.golang.org/grpc"
)

var (
	port = flag.Int("port", 50051, "The server port")
)

type server struct {
	pb.UnimplementedLinkServiceServer
}

func (s *server) CommArea(ctx context.Context, in *pb.CommReq) (out *pb.CommResp, err error) {
	start := time.Now()

	// remove trailing spaces from program name
	program := strings.TrimSpace(in.GetLinkProg())
	c_program := C.CString(program)
	defer C.free(unsafe.Pointer(c_program))

	c_commlen := C.int(in.GetCommLen())

	// allocate argc & argv variables
	c_argc := C.int(1)
	c_argv := (*[0xfff]*C.char)(C.allocArgv(c_argc))
	defer C.free(unsafe.Pointer(c_argv))

	c_argv[0] = C.CString(string(in.GetInputMsg()))
	defer C.free(unsafe.Pointer(c_argv[0]))

	// check COBOL program
	n := C.cob_resolve(c_program)
	if n == nil {
		log.Println("ERROR: Module not found. Program name =", program)
	} else {
		log.Printf("INFO: %s started", program)
		ret := C.cob_call(c_program, c_argc, (*unsafe.Pointer)(unsafe.Pointer(c_argv)))
		log.Printf("INFO: %s return-code %v", program, ret)

	}
	c_msg_output := C.GoStringN(c_argv[0], c_commlen)

	elapsed := time.Since(start)
	log.Printf("INFO: %s elapsed time %s", program, elapsed)

	return &pb.CommResp{OutputMsg: []byte(c_msg_output)}, nil
}

func main() {
	flag.Parse()

	// d8 Initialize gnucobol
	C.cob_init(C.int(0), nil)

	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", *port))
	if err != nil {
		log.Fatalf("ERROR: failed to listen: %v", err)
	}

	grpcServer := grpc.NewServer()
	pb.RegisterLinkServiceServer(grpcServer, &server{})
	log.Printf("INFO: server listening at %v", lis.Addr())
	if err := grpcServer.Serve(lis); err != nil {
		log.Fatalf("ERROR: failed to serve: %v", err)
	}
}
