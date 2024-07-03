module Infrastructure.Domain

module Errors =

    type ErrorReason = { Message: string; Code: int option }

    type Error' =
        | Web of ErrorReason
        | Persistence of string
        | Parsing of string
        | Mapping of string
        | Serialization of string
        | Configuration of string
        | Business of string
        | NotFound of string
        | NotSupported of string
        | NotImplemented of string
        | Denied of string
        | Cancelled of string

        member this.Message =
            match this with
            | Web error -> $"Web. {error}"
            | Persistence error -> $"Persistence. {error}"
            | Parsing error -> $"Parsing. {error}"
            | Mapping error -> $"Mapping. {error}"
            | Serialization error -> $"Serialization. {error}"
            | Configuration error -> $"Configuration. {error}"
            | Business error -> $"Business. {error}"
            | NotSupported source -> $"The '{source}' not supported."
            | NotImplemented source -> $"The '{source}' not implemented."
            | NotFound source -> $"The '{source}' not found."
            | Denied source -> $"The '{source}' denied."
            | Cancelled source -> $"The '{source}' task cancelled."

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
