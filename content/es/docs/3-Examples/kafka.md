---
title: COBOL & Kafka
date: 2024-06-20
description: >
  Convierta su programa COBOL en un Kafka consumer/producer.
categories: [Examples]
tags: [test, sample, docs]
weight: 50
---

Quiere integrar sus programas COBOL en un modelo de proceso basado en eventos.

Aprenda cómo convertir un programa COBOL en un Kafka consumer / producer.

Desde el programa COBOL realizaremos una llamada al módulo D8kafka pasándole:

* El _topic_ Kafka
* Una lista de valores (key : value) separados por comas

```cobol

******************************************************************
      *
      * Loan Calculator kafka
      * ==========================
      *
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cuota.
       ENVIRONMENT DIVISION.
 
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
           01 WS-LOAN.
               05 WS-AMT  PIC 9(7)V9(2).
               05 WS-INT  PIC 9(2)V9(2).
               05 WS-YEAR PIC 9(2).
      ******************************************************************  
           01 KAFKA.
               05 KAFKA-TOPIC PIC X(05) VALUE "loans".
               05 FILLER     PIC X(1)  VALUE LOW-VALUES.

              05 KAFKA-KEY.
                 10 KAFKA-KEY1 PIC X(15) VALUE "PrincipalAmount".
                 10 FILLER     PIC X(1)  VALUE ",".
                 10 KAFKA-KEY2 PIC X(12) VALUE "InterestRate".
                 10 FILLER     PIC X(1)  VALUE ",".
                 10 KAFKA-KEY1 PIC X(09) VALUE "TimeYears".
                 10 FILLER     PIC X(1)  VALUE LOW-VALUES.
              05 KAFKA-VALUE.
                 10 KAFKA-AMT-VALUE  PIC zzzzzz9.99.
                 10 FILLER     PIC X(1)  VALUE ",".
                 10 KAFKA-INT-VALUE  PIC z9.99.
                 10 FILLER     PIC X(1)  VALUE ",".
                 10 KAFKA-YEAR-VALUE PIC zz.
                 10 FILLER     PIC X(1) VALUE LOW-VALUES.

       PROCEDURE DIVISION.

           INITIALIZE WS-LOAN.

           DISPLAY "Amount: " WITH NO ADVANCING.
           ACCEPT WS-AMT.
           DISPLAY "Interest: " WITH NO ADVANCING.
           ACCEPT WS-INT.
           DISPLAY "Number of Years: " WITH NO ADVANCING.
           ACCEPT WS-YEAR.
           
           MOVE WS-AMT TO KAFKA-AMT-VALUE.
           MOVE WS-INT TO KAFKA-INT-VALUE.
           MOVE WS-YEAR TO KAFKA-YEAR-VALUE.

           CALL "D8kafka" USING KAFKA-TOPIC 
                                KAFKA-KEY
                                KAFKA-VALUE.

           DISPLAY "Return-code: " RETURN-CODE.
           
           GOBACK.
```

A continuación se muestra una versión simplificada de ejemplo del módulo _d8kafka_

```go
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

```

Para consumir el _topic_ kafka desde un programa Go puede utilizar el siguiente ejemplo:

```go
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

```

> Para ejecutar una prueba es necesario tener instalado Kafka
>
> Un manera sencilla de hacerlo es utilizar Docker (*docker-compose.yml*) para configurar un entorno mínimo de pruebas con zookeeper y kafka 




