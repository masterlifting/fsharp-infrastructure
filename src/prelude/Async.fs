[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Async

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
