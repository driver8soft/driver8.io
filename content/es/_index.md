---
title: driver8
---

{{< blocks/cover title="driver8: Migración de IBM mainframe a una arquitectura open" image_anchor="center" height="max" >}}
<a class="btn btn-lg btn-primary me-3 mb-4" href="/es/docs/">
  Documentación 
</a>
<a class="btn btn-lg btn-secondary me-3 mb-4" href="/es/docs/2-getting-started/">
  Comenzar <i class="fas fa-arrow-alt-circle-right ms-2"></i>
</a>
<p class="lead mt-5">Despliegue su código mainframe  &mdash; en la cloud!</p>
{{< blocks/link-down color="info" >}}
{{< /blocks/cover >}}

{{% blocks/lead color="primary" %}}
driver8
{.h1 .text-center}
driver8 es un proyecto basado en productos 'open source' que permite la migración del código COBOL mainframe a una plataforma nativa en la Cloud
{.h4 .text-center}
{{% /blocks/lead %}}


{{% blocks/section color="clear" type="row" %}}
{{% blocks/feature icon="fa-solid fa-bolt" title="Transacciones Online" url="https://grpc.io" %}}
Las transacciones COBOL se despliegan como microservicios gRPC

Los programas COBOL pueden llamar microservicios escritos en Java, Phyton, Go, etc. Para más información sobre los lenguajes [soportados](https://grpc.io/docs/languages/) por gRPC
{{% /blocks/feature %}}


{{% blocks/feature icon="fa-solid fa-puzzle-piece" title="Batch processes" %}}
Los JCLs se ejecutan en un planificador nativo de kubernetes, con soporte de DAG y Workflows basados en Steps

Los Workflows pueden combinar programas COBOL y programas Spark para extender y evolucionar las capacidades de los procesos
{{% /blocks/feature %}}


{{% blocks/feature icon="fa-solid fa-plug-circle-bolt" title="Mainframe proxys" %}}
Los Proxys permiten la conexión del mainframe (DB2, CICS, IMS) con Kubernetes 

Permiten una migración gradual de las aplicaciones mainframe al facilitar la convivencia 
{{% /blocks/feature %}}


{{% /blocks/section %}}
