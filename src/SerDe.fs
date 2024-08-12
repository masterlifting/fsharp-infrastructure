[<AutoOpen>]
module Infrastructure.SerDe

open System
open Infrastructure

[<RequireQualifiedAccess>]
module Json =
    open System.Text.Json
    open Infrastructure.Domain.SerDe.Json

    let private getWebApiOptions () =
        let options = JsonSerializerOptions()
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        options

    let private getStandardOptions () = JsonSerializerOptions()

    let private getDuOptions converter =
        let options = getStandardOptions ()
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
