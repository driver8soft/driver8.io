---
title: Calling COBOL containers
date: 2024-06-20
description: >
  Llame a programas COBOL remotos.
categories: [Examples]
tags: [test, sample, docs]
weight: 40
---

De manera equivalente al mecanismo del CICS para llamar a programas remotos (EXEC CICS LINK), realice llamadas entre programas COBOL desplegados en distintos contenedores.

A continuación se describe gráficamente el flujo de ejecución


*loanmain.cbl <--> d8link.go <-----------------------> main.go <--> loancalc.cbl*


1. El programa COBOL loanmain.cbl realiza una llamada (CALL) al conector gRPC _d8link_, esta simula una sentencia EXEC CICS LINK:
  * Se define el programa al que se quiere llamar
  * El área de intercambio de datos (COMMAREA)
  * Y la longitud de la misma
2. El conector gRPC _d8link_ recibe los datos (COMMAREA) y llama al microservicio COBOL correspondiente
3. El controller gPRC (_main.go_) gestiona el mensaje proto, lo convierte a una estructura compatible y llama al programa COBOL loancalc.cbl
4. El programa COBOL actualiza el área de datos y devuelve el control al controlador gRPC
5. Los datos son envíados de vuelta al conector _d8link_ que los copia sobre el área de memoria definida por el programa COBOL

Cree una estructura de directorios como la siguiente:


```
├── d8link
│   └── link_client
│   └── link_server
│   └── link
│   go.mod
│   go.sum
```

En el directorio _link_ definiremos nuestro mensaje proto (_link.proto_)

{{< readfile file="/static/img/include/d8link/d8link/link.proto" code="true" lang="proto" >}}

A continuación crearemos el programa _d8link.go_ sobre el directorio _link_client_

{{< readfile file="/static/img/include/d8link/d8link_client/d8link.go" code="true" lang="go" >}}

> Vamos a exportar la función D8link para que pueda ser llamada desde un programa COBOL, para ello es necesario compilarla utilizando la opción *c-shared* de Go
>
> El compilador de Go generará un objeto (_D8link.dylib D8link.so_) y un fichero (_D8link.h_) que serán llamados dinámicamente desde el código COBOL 

Y para finalizar crearemos el servidor gRPC (_main.go_) en el directorio *link_server* que será el encargado de recibir el mensaje proto y llamar al programa COBOL destino.

{{< readfile file="/static/img/include/d8link/d8link_server/main.go" code="true" lang="go" >}}

Pruebe a realizar llamadas remotas entre programas COBOL intercambiando un área de datos (COPYBOOK).
Para ello recuerde que:

* El programa llamador debe compilarse para generar un ejecutable (opción -x GNUCobol)
* El programa llamado debe compilarse para generar un objeto (opción -m GNUCobol)
* Ambos programas deben compilarse utilizando el mismo "byteorder" para compartir datos binarios
* Para simplificar la prueba los programas COBOL puede residir en los directorios definidos anteriormente (*link_client link_server*)

Puede utilizar los programas COBOL de ejemplo loanmain.cbl y loancalc.cbl.

{{< readfile file="/static/img/include/loanmain.cbl" code="true" lang="cobol" >}}

{{< readfile file="/static/img/include/loancalc.cbl" code="true" lang="cobol" >}}