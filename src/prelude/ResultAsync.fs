[<RequireQualifiedAccess>]
module Infrastructure.Prelude.ResultAsync

let wrap f =
    function
    | Ok x -> f x
    | Error e -> async { return Error e }

let bind f asyncResult =
    async {
        let! result = asyncResult
        return Result.bind f result
    }

let bindAsync f asyncResult =
    async {
        match! asyncResult with
        | Ok result -> return! f result
        | Error err -> return Error err
    }

let map f asyncResult =
    async {
        let! result = asyncResult
        return Result.map f result
    }

let mapAsync f asyncResult =
    async {
        match! asyncResult with
        | Ok result -> return Ok <| f result
        | Error err -> return Error err
    }

let mapError f asyncResult =
    async {
        let! result = asyncResult
        return Result.mapError f result
    }

let mapErrorAsync f asyncResult =
    async {
        match! asyncResult with
        | Ok result -> return Ok result
        | Error err ->
            let! err = f err
            return Error err
    }
    
let ignore asyncResult =
    async {
        let! _ = asyncResult
        return Ok ()
    }
