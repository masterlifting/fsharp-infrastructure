module Infrastructure.Domain

module Errors =
    type InfrastructureError =
        | InvalidResponse of string
        | InvalidRequest of string

        override this.ToString() =
            match this with
            | InvalidResponse error -> error
            | InvalidRequest error -> error

    type LogicalError =
        | NotSupported
        | NotImplemented

        override this.ToString() =
            match this with
            | NotSupported -> "Not supported"
            | NotImplemented -> "Not implemented"

    type AppError =
        | Infrastructure of InfrastructureError
        | Logical of LogicalError

        override this.ToString() =
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
        abstract member Recurcive: bool
        abstract member Handle: (CancellationTokenSource -> Async<Result<string, AppError>>) option

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
