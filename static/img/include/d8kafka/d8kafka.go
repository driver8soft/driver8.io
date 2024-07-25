package main

/*
#include <string.h>
#include <stdlib.h>
*/
import "C"
import (
	"encoding/json"
	"log"
	"os"
	"strings"

	"github.com/confluentinc/confluent-kafka-go/kafka"
)

type Kdata struct {
	Key   string `json:"key"`
	Value string `json:"value"`
}

//export D8kafka
func D8kafka(c_topic *C.char, c_key *C.char, c_value *C.char) C.int {

	logger := log.New(os.Stdout, "INFO: ", log.Ldate|log.Ltime)

	keys := strings.Split(C.GoString(c_key), ",")
	values := strings.Split(C.GoString(c_value), ",")

	data := make([]Kdata, len(keys))
	for i := 0; i < len(keys); i++ {
		data[i] = Kdata{Key: keys[i], Value: values[i]}
	}
	KafkaMsg, _ := json.Marshal(data)

	topic := C.GoString(c_topic)

	p, err := kafka.NewProducer(&kafka.ConfigMap{
		"bootstrap.servers": "localhost:29092",
		"client.id":         "cuota",
		"acks":              "all"},
	)
	if err != nil {
		logger.Fatalf("Failed to create producer: %s\n", err)
		os.Exit(1)
	}

	delivery_chan := make(chan kafka.Event, 1000)

	err = p.Produce(
		&kafka.Message{
			TopicPartition: kafka.TopicPartition{Topic: &topic, Partition: kafka.PartitionAny},
			Value:          []byte(KafkaMsg),
		},
		delivery_chan,
	)
	if err != nil {
		logger.Fatalf("Failed to produce message: %s\n", err)
		os.Exit(1)
	}
	<-delivery_chan

	return 0
}

func main() {

}
