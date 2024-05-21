module Infrastructure.Domain

open System

module Errors =
    type InfrastructureError =
        | InvalidResponse of string
        | InvalidRequest of string

        member this.message =
            match this with
            | InvalidResponse error -> error
            | InvalidRequest error -> error

    type LogicalError =
        | NotSupported
        | NotImplemented

        member this.message =
            match this with
            | NotSupported -> "Not supported"
            | NotImplemented -> "Not implemented"

    type AppError =
        | Infrastructure of InfrastructureError
        | Logical of LogicalError
        
        member this.message =
            match this with
            | Infrastructure error -> error.ToString()
            | Logical error -> error.ToString()

module Graph =
    open System.Threading
    open Errors

    type INodeName =
        abstract member Name: string

    type INodeHandle =
        inherit INodeName
        abstract member Parallel: bool
        abstract member Recursively: bool
        abstract member Duration: TimeSpan option
        abstract member Delay: TimeSpan option
        abstract member Limit: uint option
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
