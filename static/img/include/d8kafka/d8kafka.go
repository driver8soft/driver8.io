package main

/*
#include <string.h>
#include <stdlib.h>
*/
import "C"
import (
	"encoding/json"
	"fmt"
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
		"client.id":         "client",
		"acks":              "all"},
	)
	if err != nil {
		fmt.Printf("ERROR: Failed to create producer: %s\n", err)
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
		fmt.Printf("ERROR: Failed to produce message: %s\n", err)
		os.Exit(1)
	}

	e := <-delivery_chan
	m := e.(*kafka.Message)

	if m.TopicPartition.Error != nil {
		fmt.Printf("ERROR: Delivery failed: %v\n", m.TopicPartition.Error)
	} else {
		fmt.Printf("INFO: Delivered message to topic %s [%d] at offset %v\n",
			*m.TopicPartition.Topic, m.TopicPartition.Partition, m.TopicPartition.Offset)
	}
	close(delivery_chan)

	return 0
}

func main() {

}
