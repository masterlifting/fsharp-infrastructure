module Infrastructure.Domain

module Errors =
    type InfrastructureError =
        | InvalidResponse of string
        | InvalidRequest of string
        | Persistence of string
        | Parsing of string
        | Mapping of string
        | Serialization of string

        member this.Message =
            match this with
            | InvalidResponse error -> $"Invalid.{error}"
            | InvalidRequest error -> $"Invalid.{error}"
            | Persistence error -> $"Persistence.{error}"
            | Parsing error -> $"Parsing.{error}"
            | Mapping error -> $"Mapping.{error}"
            | Serialization error -> $"Serialization.{error}"

    type LogicalError =
        | NotSupported of string
        | NotImplemented of string
        | Cancelled of string

        member this.Message =
            match this with
            | NotSupported source -> $"The '{source}' is not supported."
            | NotImplemented source -> $"The '{source}' is not implemented."
            | Cancelled source -> $"The '{source}' was cancelled."

    type ApiError =
        | Infrastructure of InfrastructureError
        | Logical of LogicalError

        member this.Message =
            match this with
            | Infrastructure error -> $"Infrastructure. {error.Message}"
            | Logical error -> $"Logical. {error.Message}"

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
