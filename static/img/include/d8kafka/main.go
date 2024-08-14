package main

import (
	"fmt"
	"os"

	"github.com/confluentinc/confluent-kafka-go/kafka"
)

var topic string = "loans"
var run bool = true

func main() {
	consumer, err := kafka.NewConsumer(&kafka.ConfigMap{
		"bootstrap.servers": "localhost:29092",
		"group.id":          "sample",
		"auto.offset.reset": "smallest"},
	)
	if err != nil {
		fmt.Printf("ERROR: Failed to create consumer: %s\n", err)
		os.Exit(1)
	}

	err = consumer.Subscribe(topic, nil)
	if err != nil {
		fmt.Printf("ERROR: Failed to subscribe: %s\n", err)
		os.Exit(1)
	}

	for run {
		ev := consumer.Poll(100)
		switch e := ev.(type) {
		case *kafka.Message:
			fmt.Printf("INFO: %s", e.Value)
		case kafka.Error:
			fmt.Printf("%% ERROR: %v\n", e)
			run = false
		}
	}

	consumer.Close()

}
