﻿[<RequireQualifiedAccess>]
module Infrastructure.Prelude.AP

open System
open Infrastructure.Domain

let (|IsString|_|) (input: string | null) =
    match input with
    | null -> None
    | value ->
        match String.IsNullOrWhiteSpace value with
        | false -> Some value
        | _ -> None

let (|IsInt|_|) (input: string) =
    match Int32.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsFloat|_|) (input: string) =
    match Double.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsGuid|_|) (input: string) =
    match Guid.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsUUID16|_|) (input: string) =
    input |> validateUUID 16 |> Option.map UUID16

let (|IsUUID32|_|) (input: string) =
    input |> validateUUID 32 |> Option.map UUID32

let (|IsTimeSpan|_|) (input: string) =
    match TimeSpan.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsDateOnly|_|) (input: string) =
    match DateOnly.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsTimeOnly|_|) (input: string) =
    match TimeOnly.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsDateTime|_|) (input: string) =
    match DateTime.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsEmail|_|) (input: string) =
    match Text.RegularExpressions.Regex.IsMatch(input, @"^[^@\s]+@[^@\s]+\.[^@\s]+$") with
    | true -> Some input
    | _ -> None

let (|IsLettersOrNumbers|_|) (input: string) =
    match Text.RegularExpressions.Regex.IsMatch(input, "^[a-zA-Z0-9]+$") with
    | true -> Some input
    | _ -> None

let (|Leaf|Node|) (input: Graph.Node<_>) =
    match input.Children with
    | [] -> Leaf input.Value
    | _ -> Node input
