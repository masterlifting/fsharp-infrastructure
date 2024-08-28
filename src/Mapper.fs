[<RequireQualifiedAccess>]
module Infrastructure.Mapper

module Error =
    [<Literal>]
    let Operation = nameof Operation

    [<Literal>]
    let Permission = nameof Permission

    [<Literal>]
    let NotFound = nameof NotFound

    [<Literal>]
    let NotSupported = nameof NotSupported

    [<Literal>]
    let NotImplemented = nameof NotImplemented

    [<Literal>]
    let Cancelled = nameof Cancelled

    let toExternal error =
        let result = External.Error()

        match error with
        | Errors.Operation reason ->
            result.Type <- Operation
            result.Value <- reason.Message
            result.Code <- reason.Code
        | Errors.Permission reason ->
            result.Type <- Permission
            result.Value <- reason.Message
            result.Code <- reason.Code
        | Errors.NotFound src ->
            result.Type <- NotFound
            result.Value <- src
        | Errors.NotSupported src ->
            result.Type <- NotSupported
            result.Value <- src
        | Errors.NotImplemented src ->
            result.Type <- NotImplemented
            result.Value <- src
        | Errors.Cancelled src ->
            result.Type <- Cancelled
            result.Value <- src

        result

    let toInternal (error: External.Error) =
        match error.Type with
        | Operation ->
            Errors.Operation
                { Message = error.Value
                  Code = error.Code }
            |> Ok
        | Permission ->
            Errors.Permission
                { Message = error.Value
                  Code = error.Code }
            |> Ok
        | NotFound -> Errors.NotFound error.Value |> Ok
        | NotSupported -> Errors.NotSupported error.Value |> Ok
        | NotImplemented -> Errors.NotImplemented error.Value |> Ok
        | Cancelled -> Errors.Cancelled error.Value |> Ok
        | _ -> Error <| Errors.NotSupported "Unknown error type"
