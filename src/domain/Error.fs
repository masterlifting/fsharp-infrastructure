[<AutoOpen>]
module Infrastructure.Domain.Error

open System

module private Code =
    [<Literal>]
    let DELIMITER = ","

    [<Literal>]
    let LINE = "Line"

    [<Literal>]
    let HTTP = "Http"

    [<Literal>]
    let CUSTOM = "Custom"

module private Reason =

    [<Literal>]
    let OPERATION = "Operation"

    [<Literal>]
    let PERMISSION = "Permission"

    [<Literal>]
    let ALREADY_EXISTS = "AlreadyExists"

    [<Literal>]
    let NOT_FOUND = "NotFound"

    [<Literal>]
    let NOT_SUPPORTED = "NotSupported"

    [<Literal>]
    let NOT_IMPLEMENTED = "NotImplemented"

    [<Literal>]
    let CANCELED = "Canceled"

type ErrorCode =
    | Line of path: string * file: string * line: string
    | Http of Net.HttpStatusCode
    | Custom of string

    member this.Value =
        match this with
        | Line(path, file, line) -> $"%s{path}\\%s{file}:%s{line}"
        | Http code ->
            match code |> Enum.GetName with
            | null -> $"%A{code}"
            | name -> name
        | Custom value -> value

    member internal this.ToValue() =
        match this with
        | Line(path, file, line) -> [ Code.LINE; path; file; line ]
        | Http statusCode ->
            match statusCode |> Enum.IsDefined with
            | true ->
                match statusCode |> Enum.GetName with
                | null -> [ Code.HTTP; $"%A{statusCode}" ]
                | statusName -> [ Code.HTTP; statusName ]
            | false -> [ Code.HTTP; $"%A{statusCode}" ]
        | Custom value -> [ Code.CUSTOM; value ]
        |> String.concat Code.DELIMITER

type ErrorReason = {
    Message: string
    Code: ErrorCode option
}

type Error' =
    | Operation of ErrorReason
    | Permission of ErrorReason
    | AlreadyExists of string
    | NotFound of string
    | NotSupported of string
    | NotImplemented of string
    | Canceled of string

    member this.Message =
        match this with
        | Operation reason -> reason.Message
        | Permission reason -> reason.Message
        | AlreadyExists msg -> msg
        | NotFound msg -> msg
        | NotSupported msg -> msg
        | NotImplemented msg -> msg
        | Canceled msg -> msg

    member this.Extend value =
        match this with
        | Operation reason ->
            Operation {
                reason with
                    Message = reason.Message + value
            }
        | Permission reason ->
            Permission {
                reason with
                    Message = reason.Message + value
            }
        | AlreadyExists msg -> AlreadyExists(msg + value)
        | NotFound msg -> NotFound(msg + value)
        | NotSupported msg -> NotSupported(msg + value)
        | NotImplemented msg -> NotImplemented(msg + value)
        | Canceled msg -> Canceled(msg + value)

    member this.Replace msg =
        match this with
        | Operation reason -> Operation { reason with Message = msg }
        | Permission reason -> Permission { reason with Message = msg }
        | AlreadyExists _ -> AlreadyExists msg
        | NotFound _ -> NotFound msg
        | NotSupported _ -> NotSupported msg
        | NotImplemented _ -> NotImplemented msg
        | Canceled _ -> Canceled msg

type ErrorEntity() =
    member val Type = String.Empty with get, set
    member val Value = String.Empty with get, set
    member val Code: string option = None with get, set

    member private this.ToErrorCode() =
        match this.Code with
        | Some code ->
            let args = code.Split Code.DELIMITER

            match args.Length with
            | 2 ->
                match args[0] with
                | Code.CUSTOM -> args[1] |> Custom |> Some |> Ok
                | Code.HTTP ->
                    match Enum.TryParse<Net.HttpStatusCode>(args[1]) with
                    | true, statusCode -> statusCode |> Http |> Some |> Ok
                    | false, _ -> $"HTTP status code '{args[1]}' is not supported." |> NotSupported |> Error
                | _ ->
                    Error
                    <| Operation {
                        Message = $"Invalid error code '{code}' in the error entity."
                        Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
                    }
            | 4 ->
                match args[0] with
                | Code.LINE -> Line(args[1], args[2], args[3]) |> Some |> Ok
                | _ ->
                    Error
                    <| Operation {
                        Message = $"Invalid error code '{code}' in the error entity."
                        Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
                    }
            | _ ->
                Error
                <| Operation {
                    Message = $"Invalid error code '{code}' in the error entity."
                    Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
                }
        | None -> None |> Ok

    member this.ToDomain() =
        this.ToErrorCode()
        |> Result.bind (fun code ->
            match this.Type with
            | Reason.OPERATION -> Operation { Message = this.Value; Code = code } |> Ok
            | Reason.PERMISSION -> Permission { Message = this.Value; Code = code } |> Ok
            | Reason.ALREADY_EXISTS -> AlreadyExists this.Value |> Ok
            | Reason.NOT_FOUND -> NotFound this.Value |> Ok
            | Reason.NOT_SUPPORTED -> NotSupported this.Value |> Ok
            | Reason.NOT_IMPLEMENTED -> NotImplemented this.Value |> Ok
            | Reason.CANCELED -> Canceled this.Value |> Ok
            | _ -> $"Error type '{this.Type}' is not supported." |> NotSupported |> Error)

type Error' with
    member this.ToEntity() =
        let result = ErrorEntity()

        match this with
        | Operation reason ->
            result.Type <- Reason.OPERATION
            result.Value <- reason.Message
            result.Code <- reason.Code |> Option.map _.ToValue()
        | Permission reason ->
            result.Type <- Reason.PERMISSION
            result.Value <- reason.Message
            result.Code <- reason.Code |> Option.map _.ToValue()
        | AlreadyExists src ->
            result.Type <- Reason.ALREADY_EXISTS
            result.Value <- src
        | NotFound src ->
            result.Type <- Reason.NOT_FOUND
            result.Value <- src
        | NotSupported src ->
            result.Type <- Reason.NOT_SUPPORTED
            result.Value <- src
        | NotImplemented src ->
            result.Type <- Reason.NOT_IMPLEMENTED
            result.Value <- src
        | Canceled src ->
            result.Type <- Reason.CANCELED
            result.Value <- src

        result

type Retry<'a> = {
    Delay: int
    Attempts: uint<attempts>
    Perform: unit -> Async<Result<'a, Error'>>
}
