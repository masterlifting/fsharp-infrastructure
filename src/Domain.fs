module Infrastructure.Domain

module Errors =
    type InfrastructureError =
        | InvalidResponse of string
        | InvalidRequest of string
        | PersistenceError of string

        member this.Message =
            match this with
            | InvalidResponse error -> error
            | InvalidRequest error -> error
            | PersistenceError error -> error

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

    type INodeName =
        abstract member Name: string

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
