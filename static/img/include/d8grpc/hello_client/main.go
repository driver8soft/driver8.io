package main

import (
	"context"
	"flag"
	"log"

	pb "github.com/driver8soft/examples/d8grpc/hello"

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
	conn, err := grpc.NewClient(*addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()

	client := pb.NewD8GrpcClient(conn)

	// Contact the server and print out its response.
	r, err := client.Hello(context.Background(), &pb.MsgReq{HelloName: *name})
	if err == nil {
		log.Printf("Output: %s", r.GetResponse())
	} else {
		log.Printf("ERROR: %v", err)
	}

}
