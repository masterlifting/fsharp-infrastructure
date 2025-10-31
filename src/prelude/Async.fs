[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Async

open System
open Infrastructure.Domain

let bind next workflow =
    async {
        let! result = workflow
        return! next result
    }

let map next workflow =
    async {
        let! result = workflow
        return next result
    }

let retry (model: Retry<_>) =

    let inline increase (currentDelay: int) =
        let nextDelay = min (currentDelay * 2) 30000
        let jitterFactor = 0.2 // 20% jitter
        let jitterRange = int (float nextDelay * jitterFactor)
        if jitterRange > 0 then
            let jitter = Random().Next(-jitterRange, jitterRange)
            max 1 (nextDelay + jitter)
        else
            nextDelay

    let rec performRetry (attempts: uint<attempts>) (delay: int) =
        async {
            match! model.Perform() with
            | Ok result -> return Ok result
            | Error e ->
                match e with
                | Canceled _ -> return Error e
                | _ ->
                    match attempts = 0u<attempts> with
                    | true -> return Error e
                    | false ->
                        do! Async.Sleep delay
                        let delay = delay |> increase
                        return! performRetry (attempts - 1u<attempts>) delay
        }

    performRetry model.Attempts model.Delay
