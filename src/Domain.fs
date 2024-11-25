﻿[<AutoOpen>]
module Infrastructure.Domain

[<AutoOpen>]
module Errors =

    type ErrorReason =
        { Message: string
          Code: string option }

        static member buildLine(path, file, line) = $"%s{path}\\%s{file}:%s{line}"
        static member buildLineOpt = ErrorReason.buildLine >> Some

    type Error' =
        | Operation of ErrorReason
        | Permission of ErrorReason
        | NotFound of string
        | NotSupported of string
        | NotImplemented of string
        | Canceled of string

        member this.Message =
            match this with
            | Operation reason -> $"Operation error -> %s{reason.Message}"
            | Permission reason -> $"Permission error -> %s{reason.Message}"
            | NotFound src -> $"Not found -> %s{src}"
            | NotSupported src -> $"Not supported -> %s{src}"
            | NotImplemented src -> $"Not implemented -> %s{src}"
            | Canceled src -> $"Cancelled -> %s{src}"

        member this.MessageEx =
            match this with
            | Operation reason ->
                match reason.Code with
                | Some code -> $"Operation error -> %s{reason.Message} -> %s{code}"
                | None -> $"Operation error -> %s{reason.Message}"
            | Permission reason ->
                match reason.Code with
                | Some code -> $"Permission error -> %s{reason.Message} -> %s{code}"
                | None -> $"Permission error -> %s{reason.Message}"
            | NotFound src -> $"Not found -> %s{src}"
            | NotSupported src -> $"Not supported -> %s{src}"
            | NotImplemented src -> $"Not implemented -> %s{src}"
            | Canceled src -> $"Cancelled -> %s{src}"

        member this.add msg =
            match this with
            | Operation reason ->
                Operation
                    { reason with
                        Message = $"%s{reason.Message} -> %s{msg}" }
            | Permission reason ->
                Permission
                    { reason with
                        Message = $"%s{reason.Message} -> %s{msg}" }
            | NotFound src -> NotFound $"%s{src} -> %s{msg}"
            | NotSupported src -> NotSupported $"%s{src} -> %s{msg}"
            | NotImplemented src -> NotImplemented $"%s{src} -> %s{msg}"
            | Canceled src -> Canceled $"%s{src} -> %s{msg}"

        member this.replace msg =
            match this with
            | Operation reason -> Operation { reason with Message = msg }
            | Permission reason -> Permission { reason with Message = msg }
            | NotFound _ -> NotFound msg
            | NotSupported _ -> NotSupported msg
            | NotImplemented _ -> NotImplemented msg
            | Canceled _ -> Canceled msg


[<RequireQualifiedAccess>]
module Graph =
    open System

    [<Literal>]
    let DELIMITER = "."

    type NodeId =
        | NodeIdValue of Guid

        member this.Value =
            match this with
            | NodeIdValue id -> id

        static member parse(value: string) =
            match value |> Guid.TryParse with
            | true, id -> NodeIdValue id |> Ok
            | _ -> $"NodeId value: {value}" |> NotSupported |> Error

        static member create(value: string) = value |> Guid |> NodeIdValue

        static member New = Guid.NewGuid() |> NodeIdValue

    type INodeName =
        abstract member Id: NodeId
        abstract member Name: string
        abstract member setName: string -> INodeName

    type Node<'a when 'a :> INodeName> =
        | Node of 'a * Node<'a> list

        member this.Value =
            match this with
            | Node(current, _) -> current

        member this.Id = this.Value.Id

        member this.FullName = this.Value.Name
        member this.ShortName = DELIMITER |> this.Value.Name.Split |> Array.last

        member private this.GetChildren name =
            match this with
            | Node(_, children) ->
                children
                |> List.map (fun node ->
                    let value =
                        [ name; node.FullName ] |> String.concat DELIMITER |> node.Value.setName :?> 'a

                    let children =
                        match node with
                        | Node(_, children) -> children

                    Node(value, children))

        member this.Children = this.FullName |> this.GetChildren

module Parser =
    module Html =
        open HtmlAgilityPack

        type Page = HtmlDocument
        type Node = HtmlNode

[<AutoOpen>]
module SerDe =
    module Json =
        open System.Text.Json

        type OptionType =
            | WebApi
            | Standard
            | DU of Serialization.JsonConverter

module External =
    type Error() =
        member val Type: string = System.String.Empty with get, set
        member val Value: string = System.String.Empty with get, set
        member val Code: string option = None with get, set
