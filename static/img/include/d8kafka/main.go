package main

import (
	"log"
	"os"

	"github.com/confluentinc/confluent-kafka-go/kafka"
)

func main() {
	logger := log.New(os.Stdout, "INFO: ", log.Ldate|log.Ltime)
	topic := "loans"

	consumer, err := kafka.NewConsumer(&kafka.ConfigMap{
		"bootstrap.servers": "localhost:29092",
		"group.id":          "sample",
		"auto.offset.reset": "smallest"},
	)
	if err != nil {
		logger.Fatalf("Failed to create consumer: %s\n", err)
		os.Exit(1)
	}

	err = consumer.Subscribe(topic, nil)
	if err != nil {
		logger.Fatalf("Failed to subscribe: %s\n", err)
		os.Exit(1)
	}

	for {
		ev := consumer.Poll(1000)
		switch e := ev.(type) {
		case *kafka.Message:
			logger.Printf("%s", e.Value)
		case kafka.Error:
			logger.Fatalf("%% Error: %v\n", e)
		}
	}
}
