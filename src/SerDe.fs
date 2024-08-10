[<AutoOpen>]
module Infrastructure.SerDe

open System
open Infrastructure

[<RequireQualifiedAccess>]
module Json =
    open System.Text.Json
    open Infrastructure.Domain.SerDe.Json

    module Converter =
        open System.Text.Json.Serialization

        type Error() =
            inherit JsonConverter<Error'>()

            [<Literal>]
            let ErrorType = "Type"

            [<Literal>]
            let ErrorReason = "Reason"

            [<Literal>]
            let ErrorMessage = "Message"

            [<Literal>]
            let OperationError = "Operation"

            [<Literal>]
            let PermissionError = "Permission"

            [<Literal>]
            let NotFoundError = "NotFound"

            [<Literal>]
            let NotSupportedError = "NotSupported"

            [<Literal>]
            let NotImplementedError = "NotImplemented"

            [<Literal>]
            let CancelledError = "Cancelled"

            override _.Write(writer: Utf8JsonWriter, value: Error', options: JsonSerializerOptions) =
                writer.WriteStartObject()

                match value with
                | Operation reason ->
                    writer.WriteString(ErrorType, OperationError)
                    writer.WritePropertyName(ErrorReason)
                    JsonSerializer.Serialize(writer, reason, options)
                | Permission reason ->
                    writer.WriteString(ErrorType, PermissionError)
                    writer.WritePropertyName(ErrorReason)
                    JsonSerializer.Serialize(writer, reason, options)
                | NotFound msg ->
                    writer.WriteString(ErrorType, NotFoundError)
                    writer.WriteString(ErrorMessage, msg)
                | NotSupported msg ->
                    writer.WriteString(ErrorType, NotSupportedError)
                    writer.WriteString(ErrorMessage, msg)
                | NotImplemented msg ->
                    writer.WriteString(ErrorType, NotImplementedError)
                    writer.WriteString(ErrorMessage, msg)
                | Cancelled msg ->
                    writer.WriteString(ErrorType, CancelledError)
                    writer.WriteString(ErrorMessage, msg)

                writer.WriteEndObject()

            override _.Read(reader: byref<Utf8JsonReader>, typeToConvert: Type, options: JsonSerializerOptions) =
                if reader.TokenType <> JsonTokenType.StartObject then
                    raise <| JsonException("Expected StartObject")

                let mutable errorType = ""
                let mutable errorReason: ErrorReason option = None
                let mutable errorMessage = ""

                while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                    match reader.TokenType with
                    | JsonTokenType.PropertyName ->
                        let propertyName = reader.GetString()
                        reader.Read() |> ignore
                        match propertyName with
                        | ErrorType -> errorType <- reader.GetString()
                        | ErrorReason -> errorReason <- JsonSerializer.Deserialize<ErrorReason>(reader.GetString(), options) |> Some
                        | ErrorMessage -> errorMessage <- reader.GetString()
                        | _ -> reader.Skip()
                    | _ -> reader.Skip()

                match errorType with
                | OperationError -> 
                    match errorReason with
                    | Some reason -> Operation reason
                    | None -> raise <| JsonException("Missing Reason for Operation Error")
                | PermissionError ->
                    match errorReason with
                    | Some reason -> Permission reason
                    | None -> raise <| JsonException("Missing Reason for Permission Error")
                | NotFoundError -> NotFound errorMessage
                | NotSupportedError -> NotSupported errorMessage
                | NotImplementedError -> NotImplemented errorMessage
                | CancelledError -> Cancelled errorMessage
                | _ -> raise <| JsonException($"Unknown Error type: {errorType}")

    let private getWebApiOptions () =
        let options = JsonSerializerOptions()
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        options

    let private getStandardOptions () = JsonSerializerOptions()

    let private getDuOptions converter =
        let options = getStandardOptions ()
        options.Converters.Add (Converter.Error())
        options.Converters.Add converter
        options

    let serialize data =
        try
            Ok <| JsonSerializer.Serialize data
        with ex ->
            Error <| NotSupported ex.Message

    let serialize' optionsType data =

        let options =
            match optionsType with
            | WebApi -> getWebApiOptions ()
            | Standard -> getStandardOptions ()
            | DU converter -> getDuOptions converter

        try
            Ok <| JsonSerializer.Serialize(data, options)
        with ex ->
            Error <| NotSupported ex.Message

    let deserialize<'a> (data: string) =
        try
            Ok <| JsonSerializer.Deserialize<'a> data
        with ex ->
            Error <| NotSupported ex.Message

    let deserialize'<'a> optionsType (data: string) =

        let options =
            match optionsType with
            | WebApi -> getWebApiOptions ()
            | Standard -> getStandardOptions ()
            | DU converter -> getDuOptions converter

        try
            Ok <| JsonSerializer.Deserialize<'a>(data, options)
        with ex ->
            Error <| NotSupported ex.Message

[<RequireQualifiedAccess>]
module Yaml =
    open YamlDotNet.Serialization

    let private serializer = SerializerBuilder().Build()
    let private deserializer = DeserializerBuilder().Build()

    let serialize data =
        try
            Ok <| serializer.Serialize data
        with ex ->
            Error <| NotSupported ex.Message

    let deserialize<'a> (data: string) =
        try
            Ok <| deserializer.Deserialize<'a> data
        with ex ->
            Error <| NotSupported ex.Message
