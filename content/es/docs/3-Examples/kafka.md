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

{{< readfile file="/static/img/include/d8kafka/cuotak.cbl" code="true" lang="cobol" >}}

A continuación se muestra una versión simplificada de ejemplo del módulo _d8kafka_

{{< readfile file="/static/img/include/d8kafka/d8kafka.go" code="true" lang="go" >}}

Para consumir el _topic_ kafka desde un programa Go puede utilizar el siguiente ejemplo:

{{< readfile file="/static/img/include/d8kafka/main.go" code="true" lang="go" >}}

> Para ejecutar una prueba es necesario tener instalado Kafka
>
> Un manera sencilla de hacerlo es utilizar Docker (*docker-compose.yml*) para configurar un entorno mínimo de pruebas con zookeeper y kafka