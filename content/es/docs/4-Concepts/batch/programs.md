---
title: Compilación programas
date: 2017-01-05
description: >
 ¿Cómo reutilizar los programas de aplicación del mainframe?
categories: [Concepts]
weight: 43
---

Los programas COBOL PL/I de la plataforma mainframe son directamente reutilizables sobre la plataforma técnica de destino (Linux).

Como hemos comentado anteriormente, el módulo _d8parti_ será el encargado de:

* Inicializar el “runtime” del lenguaje (i.e. COBOL)
* Asignar los ficheros de entrada/salida a los nombres simbólicos del programa
* Cargar y ejecutar el programa principal (definido en la ficha EXEC del JCL)


> Este programa principal puede realizar distintas llamadas a otras subrutinas mediante una sentencia CALL, estas llamadas son gestionadas por el “runtime” del lenguaje utilizado


Podemos visualizar este funcionamiento como un árbol invertido


![](/img/others/arch-batch2-v1.0.png)


Los programas compilados pueden almacenarse en un directorio compartido y cargarse en tiempo de ejecución (CALL dinámico) replicando el funcionamiento del mainframe IBM (STEPLIB). 

Sin embargo, existe la posibilidad de cambiar el comportamiento anterior e implementar un modelo de contenedores inmutables, que presenta ciertas ventajas respecto al modelo anterior. En este caso, el árbol de ejecución anterior debería descomponerse funcionalmente en uno o varios “repos”.

Las modificaciones en alguno de los componentes de estos “repos” generan una nueva versión del mismo y la consiguiente regeneración del contenedor o contenedores que lo utilicen.

Con esta estrategia conseguimos:

* Facilitar el proceso de desarrollo y prueba de las aplicaciones
* Permitir la introducción progresiva de cambios en el sistema, eliminando riesgos
* Posibilitar la portabilidad de los procesos a distintas plataformas (On-prem, On-cloud)

Una vez aislada una función de negocio en un contenedor con una interfaz estándar, este puede modificarse o re-escribirse en cualquier otro lenguaje de programación y desplegarse de manera transparente sin afectar al resto del sistema.   
