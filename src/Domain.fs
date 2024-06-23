module Infrastructure.Domain

module Errors =
    type InfrastructureError =
        | WebError of string
        | PersistenceError of string
        | ParsingError of string
        | MappingError of string
        | SerializationError of string
        | ConfigurationError of string

        member this.Message =
            match this with
            | WebError error -> $"Web.{error}"
            | PersistenceError error -> $"Persistence.{error}"
            | ParsingError error -> $"Parsing.{error}"
            | MappingError error -> $"Mapping.{error}"
            | SerializationError error -> $"Serialization.{error}"
            | ConfigurationError error -> $"Configuration.{error}"

    type LogicalError =
        | BusinessError of string
        | NotSupportedError of string
        | NotImplementedError of string
        | CancelledError of string

        member this.Message =
            match this with
            | BusinessError error -> error
            | NotSupportedError source -> $"The '{source}' is not supported."
            | NotImplementedError source -> $"The '{source}' is not implemented."
            | CancelledError source -> $"The '{source}' was cancelled."

    type ErrorType =
        | InfrastructureError of InfrastructureError
        | LogicalError of LogicalError

        member this.Message =
            match this with
            | InfrastructureError error -> $"Infrastructure. {error.Message}"
            | LogicalError error -> $"Logical. {error.Message}"

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
