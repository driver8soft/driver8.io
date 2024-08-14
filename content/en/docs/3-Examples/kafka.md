---
title: COBOL & Kafka
date: 2024-06-20
description: >
  Turn your COBOL program into a Kafka consumer/producer.
categories: [Examples]
tags: [test, sample, docs]
weight: 50
---

Leverage your COBOL programs into an event-driven process model.

Learn how to convert a COBOL program into a Kafka consumer/producer.

From the COBOL program, we will make a call to the D8kafka module and pass it:

* The Kafka _topic_
* A comma-separated list of values (key : value)

{{< readfile file="/static/img/include/d8kafka/cuotak.cbl" code="true" lang="cobol" >}}

A simplified example of _d8kafka_ is shown below.

{{< readfile file="/static/img/include/d8kafka/d8kafka.go" code="true" lang="go" >}}

To consume the kafka _topic_ from a Go program you can use the following example:

{{< readfile file="/static/img/include/d8kafka/main.go" code="true" lang="go" >}}

> You must have Kafka installed to run a test.
>
> An easy way to do this is to use Docker (*docker-compose.yml*) to set up a minimal test environment with Zookeeper and Kafka.