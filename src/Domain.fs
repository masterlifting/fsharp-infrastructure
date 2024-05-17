module Infrastructure.Domain

module Errors =
    type InfrastructureError =
        | InvalidResponse of string
        | InvalidRequest of string

    type LogicError =
        | NotSupported
        | NotImplemented

    type AppError =
        | InfrastructureError of InfrastructureError
        | LogicError of LogicError

module Graph =

    type INodeName =
        abstract member Name: string

    type INodeHandle =
        inherit INodeName
        abstract member Parallel: bool
        abstract member Recurcive: bool
        abstract member Handle: (unit -> Async<Result<string, string>>) option

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
