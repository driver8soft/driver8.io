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
	"errors"
	"flag"
	"fmt"
	"log"
	"net"
	"time"
	"unsafe"

	pb "github.com/driver8soft/examples/d8grpc/hello"

	"google.golang.org/grpc"
)

var (
	port = flag.Int("port", 50051, "The server port")
)

type server struct {
	pb.UnimplementedD8GrpcServer
}

func (s *server) Hello(ctx context.Context, in *pb.MsgReq) (out *pb.MsgRes, err error) {
	start := time.Now()

	// define argc, argv
	c_argc := C.int(1)
	c_argv := (*[0xfff]*C.char)(C.allocArgv(c_argc))
	defer C.free(unsafe.Pointer(c_argv))
	c_argv[0] = C.CString(in.GetHelloName())

	// check COBOL program
	n := C.cob_resolve(C.CString("hello"))
	if n == nil {
		err := errors.New("COBOL: program not found")
		log.Println(err)
		return &pb.MsgRes{}, err
	}

	//Call COBOL program
	log.Println("INFO: program hello started")
	ret := C.cob_call(C.CString("hello"), c_argc, (*unsafe.Pointer)(unsafe.Pointer(c_argv)))
	log.Printf("INFO: program hello return-code %v", ret)

	//COBOL COPYBOOK is converted to Go String using COPYBOOK length
	output := C.GoStringN(c_argv[0], 27)

	elapsed := time.Since(start)
	log.Printf("INFO: Hello elapsed time %s", elapsed)

	return &pb.MsgRes{Response: output[9:]}, nil

}

func main() {
	flag.Parse()
	// d8 Initialize gnucobol
	C.cob_init(C.int(0), nil)

	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", *port))
	if err != nil {
		log.Fatalf("ERROR: failed to listen: %v", err)
	}

	var opts []grpc.ServerOption

	grpcServer := grpc.NewServer(opts...)

	pb.RegisterD8GrpcServer(grpcServer, &server{})
	log.Printf("INFO: server listening at %v", lis.Addr())
	if err := grpcServer.Serve(lis); err != nil {
		log.Fatalf("ERROR: failed to serve: %v", err)
	}
}
