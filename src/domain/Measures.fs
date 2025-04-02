[<AutoOpen>]
module Infrastructure.Domain.Measures

[<Measure>] type day
[<Measure>] type attempts

type RetryPolicy = { Limit: uint<attempts / day> }


[<Measure>] type usd
[<Measure>] type euro

type Currency =
    | USD of decimal<usd>
    | Euro of decimal<euro>


let convertToUsd (currency: Currency) =
    match currency with
    | USD amount -> amount
    | Euro amount -> amount * 1.11m<usd/euro>
