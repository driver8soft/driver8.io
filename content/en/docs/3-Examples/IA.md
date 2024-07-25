---
title: COBOL to Go
date: 2024-06-20
description: >
  How to convert COBOL code to Go.
categories: [Examples]
tags: [test, sample, docs]
weight: 70
---

Advances in AI Gen offer a glimpse of a future where code conversion between different programming languages can be done automatically and transparently.

However, the characteristics of the COBOL language must be taken into account in order to select an option that preserves the converted code structure so that it can continue to be maintained by the team in charge.

Let us take the example of a COBOL routine that calculates the instalment of a loan.

{{< readfile file="/static/img/include/loancalc.cbl" code="true" lang="cobol" >}}

A first approach is to preserve the structure of the COBOL code:
* A COBOL subroutine is equivalent to a Go function. 
* The variables defined in the WORKING STORAGE are grouped and converted into Go variables. 
* The PROCEDURE DIVISION code is made up of one or more sections (PARAGRAPHS), which in turn can be transformed into very simple functions.
* Finally, the LINKAGE SECTION variables define the parameters of the main function and are shared (pointers) between all the functions.

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

With gRPC, the COBOL code has already been exposed through a standard interface that defines the input/output parameters of the function (e.g. through a proto message).

By defining such an interface, it is possible to refactor the code, simplifying the end result.

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



