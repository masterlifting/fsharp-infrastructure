[<AutoOpen>]
module Infrastructure.Domain

[<AutoOpen>]
module Errors =

    type ErrorReason =
        { Message: string
          Code: string option }
        static member buildLine (path,file,line) = $"{path}\\{file}:{line}" |> Some

    type Error' =
        | Operation of ErrorReason
        | Permission of ErrorReason
        | NotFound of string
        | NotSupported of string
        | NotImplemented of string
        | Cancelled of string

        member this.Message =
            match this with
            | Operation reason ->
                match reason.Code with
                | Some code -> $"Operation error: {reason.Message} ({code})"
                | None -> $"Operation error: {reason.Message}"
            | Permission reason ->
                match reason.Code with
                | Some code -> $"Permission error: {reason.Message} ({code})"
                | None -> $"Permission error: {reason.Message}"
            | NotFound src -> $"Not found: {src}"
            | NotSupported src -> $"Not supported: {src}"
            | NotImplemented src -> $"Not implemented: {src}"
            | Cancelled src -> $"Cancelled: {src}"

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
        open System.Text.Json

        type OptionType =
            | WebApi
            | Standard
            | DU of Serialization.JsonConverter

module External =
    type Error() =
        [<Literal>]
        static let Operation = "Operation"

        [<Literal>]
        static let Permission = "Permission"

        [<Literal>]
        static let NotFound = "NotFound"

        [<Literal>]
        static let NotSupported = "NotSupported"

        [<Literal>]
        static let NotImplemented = "NotImplemented"

        [<Literal>]
        static let Cancelled = "Cancelled"

        member val Type: string = System.String.Empty with get, set
        member val Value: string = System.String.Empty with get, set
        member val Code: string option = None with get, set

        static member fromDU error =
            let result = Error()

            match error with
            | Errors.Operation reason ->
                result.Type <- Operation
                result.Value <- reason.Message
                result.Code <- reason.Code
            | Errors.Permission reason ->
                result.Type <- Permission
                result.Value <- reason.Message
                result.Code <- reason.Code
            | Errors.NotFound src ->
                result.Type <- NotFound
                result.Value <- src
            | Errors.NotSupported src ->
                result.Type <- NotSupported
                result.Value <- src
            | Errors.NotImplemented src ->
                result.Type <- NotImplemented
                result.Value <- src
            | Errors.Cancelled src ->
                result.Type <- Cancelled
                result.Value <- src

            result

        member this.toDU() =
            match this.Type with
            | Operation ->
                Errors.Operation
                    { Message = this.Value
                      Code = this.Code }
                |> Ok
            | Permission ->
                Errors.Permission
                    { Message = this.Value
                      Code = this.Code }
                |> Ok
            | NotFound -> Errors.NotFound this.Value |> Ok
            | NotSupported -> Errors.NotSupported this.Value |> Ok
            | NotImplemented -> Errors.NotImplemented this.Value |> Ok
            | Cancelled -> Errors.Cancelled this.Value |> Ok
            | _ -> Result.Error <| Errors.NotSupported "Unknown error type"
