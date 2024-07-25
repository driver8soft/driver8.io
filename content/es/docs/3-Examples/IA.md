---
title: COBOL to Go
date: 2024-06-20
description: >
  Convierta el código COBOL a Go.
categories: [Examples]
tags: [test, sample, docs]
weight: 70
---

Los avances en IA Gen permiten vislumbrar un futuro en el que la conversión de código entre distintos lenguajes de programación pueda realizarse de manera automática y transparente.

Sin embargo, deben tenerse en cuenta las características del lenguaje COBOL para seleccionar una opción que permita reconocer el código convertido de forma que pueda seguir siendo mantenido por el equipo responsable.

Vamos a utilizar la rutina COBOL de ejemplo que calcula la cuota de un préstamo.

{{< readfile file="/static/img/include/loancalc.cbl" code="true" lang="cobol" >}}

Una primera aproximación pasa por mantener la estructura del código COBOL:
* Una subrutina COBOL equivale a una función en Go 
* Las variables definidas en la WORKING-STORAGE se agrupan y transforman en variables Go 
* El código de la PROCEDUCE DIVISION se compone de uno o varias secciones (PARAGRAPHS), estas se pueden transformar a su vez en funciones muy sencillas
* Por último, las variables de la LINKAGE SECTION definen los parámetros de la función principal y se comparten (puntero) entre todas las funciones

```go
/// Declare variables in the working storage section
var (
	WS_ERROR     string
	WS_MSG00     string = "OK"
	WS_MSG10     string = "INVALID INT. RATE"
	WS_MSG12     string = "INVALID NUMBER YEARS"
	MONTHLY_RATE float32
	AUX_X        float32
	AUX_Y        float32
	AUX_Z        float32
)

// Data to share with COBOL subroutines
type LoanParams struct {
	PrinAmt  float32
	IntRate  float32
	TimeYr   int32
	Payment  float32
	ErrorMsg string
}

func loancalc(amount float32, interest float32, nyears int32) (payment float32, errmsg string) {
	WS_ERROR = "N"

	loanParams := LoanParams{
		PrinAmt: amount,
		IntRate: interest,
		TimeYr:  nyears,
	}

	fmt.Println("PRIN-AMT:", loanParams.PrinAmt)
	fmt.Println("INT-RATE:", loanParams.IntRate)
	fmt.Println("TIMEYR:", loanParams.TimeYr)

	initial(&loanParams)
	if WS_ERROR == "N" {
		process(&loanParams)
	}
	wrapup(&loanParams)

	return loanParams.Payment, loanParams.ErrorMsg
}

func initial(loanParams *LoanParams) {
	if loanParams.IntRate <= 0 {
		loanParams.ErrorMsg = WS_MSG10
		WS_ERROR = "Y"
	} else {
		if loanParams.TimeYr <= 0 {
			loanParams.ErrorMsg = WS_MSG12
			WS_ERROR = "Y"
		}
	}
}

func process(loanParams *LoanParams) {
	MONTHLY_RATE = loanParams.IntRate / 12 / 100
	AUX_X = float32(math.Pow(float64(1+MONTHLY_RATE), float64(loanParams.TimeYr*12)))
	AUX_Y = float32(AUX_X) * MONTHLY_RATE
	AUX_Z = float32(AUX_X-1) / AUX_Y
	loanParams.Payment = loanParams.PrinAmt / AUX_Z
	loanParams.ErrorMsg = WS_MSG00
}

func wrapup(loanParams *LoanParams) {
	fmt.Println("PAYMENT:", loanParams.Payment)
	fmt.Println("ERROR-MSG:", loanParams.ErrorMsg)
}
```

Hay que recordar que el código COBOL ya ha sido expuesto mediante una interfaz estándar que define los parámetros de entrada/salida de la función (por ejemplo, mediante un mensaje proto).

Utilizando la definición de dicha interfaz, es posible volver a refactorizar el código simplificando el resultado final.


```go 
func loancalc(amount, interest float32, nyears int32) (payment float32, errmsg string) {
	if interest <= 0 {
		return 0, "Invalid int. rate"
	}
	if nyears <= 0 {
		return 0, "Invalid number of years"
	}

	monthlyRate := (interest / 12 / 100)
	x := math.Pow(float64(1+monthlyRate), float64(nyears*12))
	y := float32(x) * monthlyRate
	payment = amount / (float32(x-1) / y)

	return payment, "OK"
}
```



