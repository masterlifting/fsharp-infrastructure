module Infrastructure.Domain

module Errors =
    type InfrastructureError =
        | InvalidResponse of string
        | InvalidRequest of string

        member this.Message =
            match this with
            | InvalidResponse error -> error
            | InvalidRequest error -> error

    type LogicalError =
        | NotSupported
        | NotImplemented
        | Cancelled of string

        member this.Message =
            match this with
            | NotSupported -> "Not supported"
            | NotImplemented -> "Not implemented"
            | Cancelled task -> $"Task '{task}' was cancelled"

    type AppError =
        | Infrastructure of InfrastructureError
        | Logical of LogicalError

        member this.Message =
            match this with
            | Infrastructure error -> error.Message
            | Logical error -> error.Message

module Graph =
    open System.Threading
    open Errors
    open System

    type INodeName =
        abstract member Name: string

    type INodeHandle =
        inherit INodeName
        abstract member Parallel: bool
        abstract member Recursively: bool
        abstract member Duration: TimeSpan option
        abstract member Handle: (CancellationToken -> Async<Result<string, AppError>>) option

    type Node<'a when 'a :> INodeName> =
        | Node of 'a * Node<'a> list

        member this.Deconstructed =
            match this with
            | Node(current, children) -> (current, children)

        member this.Value =
            match this with
            | Node(current, _) -> current

        member this.Children =
            match this with
            | Node(_, children) -> children
