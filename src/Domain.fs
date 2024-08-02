[<AutoOpen>]
module Infrastructure.Domain

[<AutoOpen>]
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
            | Operation error -> $"Operation error: {error.Message}"
            | Permission error -> $"Permission error: {error.Message}"
            | NotFound msg -> $"Not found: {msg}"
            | NotSupported msg -> $"Not supported: {msg}"
            | NotImplemented source -> $"Not implemented: {source}"
            | Cancelled source -> $"Cancelled: {source}"

[<RequireQualifiedAccess>]
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

module Parser =
    module Html =
        open HtmlAgilityPack

        type Page = HtmlDocument
        type Node = HtmlNode

[<AutoOpen>]
module SerDe =
    module Json =
        type OptionType =
            | WebApi
            | Standard
