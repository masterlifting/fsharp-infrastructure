﻿[<AutoOpen>]
module Infrastructure.Domain.Error

open System

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

type Retry<'a> = {
    Delay: int
    Attempts: uint<attempts>
    Perform: unit -> Async<Result<'a, Error'>>
}
