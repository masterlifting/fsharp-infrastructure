[<RequireQualifiedAccess>]
module Infrastructure.Prelude.ResultAsync

let wrap f =
    function
    | Ok x -> f x
    | Error e -> async { return Error e }

let bind f workflow =
    async {
        let! result = workflow
        return Result.bind f result
    }

let bindAsync f workflow =
    async {
        match! workflow with
        | Ok result -> return! f result
        | Error err -> return Error err
    }

let map f workflow =
    async {
        let! result = workflow
        return Result.map f result
    }

let mapAsync f workflow =
    async {
        match! workflow with
        | Ok result -> return Ok <| f result
        | Error err -> return Error err
    }

let mapError f workflow =
    async {
        let! result = workflow
        return Result.mapError f result
    }

let mapErrorAsync f workflow =
    async {
        match! workflow with
        | Ok result -> return Ok result
        | Error err ->
            let! err = f err
            return Error err
    }

let defaultWith f workflow =
    async {
        let! result = workflow
        return Result.defaultWith f result
    }

let apply f workflow =
    async {
        match! workflow with
        | Ok w ->
            match f with
            | Ok() -> return Ok w
            | Error errF -> return Error errF
        | Error error ->
            match f with
            | Ok() -> return Error error
            | Error errF -> return Error errF
    }

let applyAsync f workflow =
    async {
        match! workflow with
        | Ok w ->
            match! f with
            | Ok() -> return Ok w
            | Error errF -> return Error errF
        | Error error ->
            match! f with
            | Ok() -> return Error error
            | Error errF -> return Error errF
    }
