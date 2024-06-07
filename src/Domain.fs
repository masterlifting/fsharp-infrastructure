﻿module Infrastructure.Domain

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
            | InvalidResponse error -> error
            | InvalidRequest error -> error
            | Persistence error -> error
            | Parsing error -> error
            | Mapping error -> error
            | Serialization error -> error

    type LogicalError =
        | NotSupported of string
        | NotImplemented of string
        | Cancelled of string

        member this.Message =
            match this with
            | NotSupported source -> $"{source} not supported."
            | NotImplemented source -> $"{source} not implemented."
            | Cancelled source -> $"{source}' was cancelled."

    type ApiError =
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
