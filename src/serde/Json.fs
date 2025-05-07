module Infrastructure.SerDe.Json

open System.Text.Json
open System.Runtime.Serialization
open Infrastructure.Domain
open Infrastructure.Prelude

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
