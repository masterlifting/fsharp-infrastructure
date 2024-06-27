module Infrastructure.Domain

module Errors =
    type Error' =
        | Web of string
        | Persistence of string
        | Parsing of string
        | Mapping of string
        | Serialization of string
        | Configuration of string
        | Business of string
        | NotSupported of string
        | NotImplemented of string
        | Cancelled of string

        member this.Message =
            match this with
            | Web error -> $"Web: {error}"
            | Persistence error -> $"Persistence: {error}"
            | Parsing error -> $"Parsing: {error}"
            | Mapping error -> $"Mapping: {error}"
            | Serialization error -> $"Serialization: {error}"
            | Configuration error -> $"Configuration: {error}"
            | Business error -> $"Business: {error}"
            | NotSupported source -> $"'{source}' is not supported."
            | NotImplemented source -> $"'{source}' is not implemented."
            | Cancelled source -> $"'{source}' task was cancelled."

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
