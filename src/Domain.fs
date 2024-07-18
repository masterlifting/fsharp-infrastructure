module Infrastructure.Domain

module Errors =

    type ErrorReason =
        { Message: string; Code: string option }

    type Error' =
        | Operation of ErrorReason
        | Permission of ErrorReason
        | NotFound of string
        | NotSupported of string
        | NotImplemented of string
        | Cancelled of string

        member this.Message =
            match this with
            | Operation error ->
                match error.Code with
                | Some code -> $"Code: {code}; Operation error: {error.Message} "
                | None -> $"Operation error: {error.Message}"
            | Permission error ->
                match error.Code with
                | Some code -> $"Code: {code}; Permission error: {error.Message}"
                | None -> $"Permission error: {error.Message}"
            | NotFound msg -> $"Not found: {msg}"
            | NotSupported msg -> $"Not supported: {msg}"
            | NotImplemented source -> $"Not implemented: {source}"
            | Cancelled source -> $"Cancelled: {source}"

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
