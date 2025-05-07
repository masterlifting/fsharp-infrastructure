[<AutoOpen>]
module Infrastructure.SerDe

open System.Runtime.Serialization
open Infrastructure.Domain
open Infrastructure.Prelude

[<RequireQualifiedAccess>]
module Json =
    open System.Text.Json

    let serialize data =
        try
            Ok <| JsonSerializer.Serialize data
        with ex ->
            Error
            <| Operation {
                Message = ex |> Exception.toMessage
                Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
            }

    let serialize' (options: JsonSerializerOptions) data =
        try
            Ok <| JsonSerializer.Serialize(data, options)
        with ex ->
            Error
            <| Operation {
                Message = ex |> Exception.toMessage
                Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
            }

    let deserialize<'a> (data: string) =
        try
            match JsonSerializer.Deserialize<'a> data with
            | null -> raise <| SerializationException $"Deserialization failed for '%A{data}'."
            | value -> Ok value
        with ex ->
            Error
            <| Operation {
                Message = ex |> Exception.toMessage
                Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
            }

    let deserialize'<'a> (options: JsonSerializerOptions) (data: string) =
        try
            match JsonSerializer.Deserialize<'a>(data, options) with
            | null -> raise <| SerializationException $"Deserialization failed for '%s{data}'."
            | value -> Ok value
        with ex ->
            Error
            <| Operation {
                Message = ex |> Exception.toMessage
                Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
            }

[<RequireQualifiedAccess>]
module Yaml =
    open YamlDotNet.Serialization

    let private serializer = SerializerBuilder().Build()
    let private deserializer = DeserializerBuilder().Build()

    let serialize data =
        try
            Ok <| serializer.Serialize data
        with ex ->
            Error
            <| Operation {
                Message = ex |> Exception.toMessage
                Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
            }

    let deserialize<'a> (data: string) =
        try
            Ok <| deserializer.Deserialize<'a> data
        with ex ->
            Error
            <| Operation {
                Message = ex |> Exception.toMessage
                Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
            }
