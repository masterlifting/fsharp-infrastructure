﻿module Infrastructure.SerDe.Yaml

open YamlDotNet.Serialization
open Infrastructure.Domain
open Infrastructure.Prelude

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
