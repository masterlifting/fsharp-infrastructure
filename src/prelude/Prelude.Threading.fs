[<AutoOpen>]
module Infrastructure.Prelude.Threading

open System.Threading

let canceled (cToken: CancellationToken) = cToken.IsCancellationRequested
let notCanceled (cToken: CancellationToken) = not <| cToken.IsCancellationRequested

