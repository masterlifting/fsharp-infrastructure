[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Async

open System
open Infrastructure.Domain

let bind next asyncWorkflow =
    async {
        let! result = asyncWorkflow
        return! next result
    }

let map next asyncWorkflow =
    async {
        let! result = asyncWorkflow
        return next result
    }

let apply f =
    bind (fun asyncWorkflow ->
        f
        |> map (function
            | Ok _ -> asyncWorkflow
            | Error error -> Error error))

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
                if attempts = 0u<attempts> then
                    return Error e
                else
                    do! Async.Sleep delay
                    let delay = delay |> increase
                    return! performRetry (attempts - 1u<attempts>) delay
        }

    performRetry model.Attempts model.Delay
