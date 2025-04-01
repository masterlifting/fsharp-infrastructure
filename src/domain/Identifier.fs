[<AutoOpen>]
module Infrastructure.Domain.Identifier

open System

let private createUUID size (guid: Guid) = guid.ToString("N").Substring(0, size)
let private validateUUID size (uuid: string) =
    match uuid.Length <> size with
    | true -> None
    | false ->
        match uuid |> Seq.forall Char.IsAsciiHexDigit with 
        | true -> Some uuid
        | false -> None

type UUID16 =
    | UUID16 of string

    member this.Value =
        match this with
        | UUID16 id -> id

    static member parse(input: string) = input |> validateUUID 16 |> Option.map UUID16

    static member convert guid = guid |> createUUID 16 |> UUID16

    static member createNew() =
        Guid.CreateVersion7() |> createUUID 16 |> UUID16

type UUID32 =
    | UUID32 of string

    member this.Value =
        match this with
        | UUID32 id -> id

    static member parse(input: string) = input |> validateUUID 32 |> Option.map UUID32

    static member convert guid = guid |> createUUID 32 |> UUID32

    static member createNew() =
        Guid.CreateVersion7() |> createUUID 32 |> UUID32
