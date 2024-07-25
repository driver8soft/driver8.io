package main

/*
#include <string.h>
#include <stdlib.h>
*/
import "C"
import (
	"context"
	"flag"
	"log"
	"time"
	"unsafe"

	pb "examples/link/link"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

var (
	addr = flag.String("addr", "localhost:50051", "the address to connect to")
)

//export D8link
func D8link(c_program *C.char, c_commarea *C.char, c_commlen *C.int) C.int {
	flag.Parse()

	//C variables to Go variables
	program := C.GoStringN(c_program, 8) //max length of COBOL mainframe program = 8
	commarea := C.GoBytes(unsafe.Pointer(c_commarea), *c_commlen)
	commlen := int32(*c_commlen)

	log.Println("INFO: Call program -", program)

	conn, err := grpc.Dial(*addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("ERROR: did not connect: %v", err)
	}
	defer conn.Close()

	c := pb.NewLinkServiceClient(conn)

	// Contact the server
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	r, err := c.CommArea(ctx, &pb.CommReq{LinkProg: program, CommLen: commlen, InputMsg: commarea})
	if err != nil {
		log.Fatalf("ERROR: calling program - %s - %v", program, err)
	}

	outMsg := r.GetOutputMsg()

	C.memcpy(unsafe.Pointer(c_commarea), unsafe.Pointer(&outMsg[0]), C.size_t(commlen))

	return 0
}

func main() {
}
