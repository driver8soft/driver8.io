package main

import (
	"context"
	"flag"
	"log"
	"time"

	pb "examples/hello/hello"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

var (
	addr = flag.String("addr", "localhost:50051", "the address to connect to")
)

var (
	name = flag.String("name", "", "name")
)

func main() {
	flag.Parse()
	// Set up a connection to the server.
	conn, err := grpc.Dial(*addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()
	c := pb.NewD8GrpcClient(conn)

	// Contact the server and print out its response.
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	r, err := c.Hello(ctx, &pb.MsgReq{HelloName: *name})
	if err == nil {
		log.Printf("Output: %s", r.GetResponse())
	} else {
		log.Printf("ERROR: %v", err)
	}

}
