[<AutoOpen>]
module Infrastructure.Domain.Identifier

open System

let private createUUID size (guid: Guid) = guid.ToString("N").Substring(0, size)

type UUID16 =
    | UUID16 of string

    member this.Value =
        match this with
        | UUID16 id -> id

    //TODO: Implement parsing
    static member parse(input: string) = UUID16 input |> Some

    static member convert guid = guid |> createUUID 16 |> UUID16

    static member createNew() =
        Guid.CreateVersion7() |> createUUID 16 |> UUID16

type UUID32 =
    | UUID32 of string

    member this.Value =
        match this with
        | UUID32 id -> id

    //TODO: Implement parsing
    static member parse(input: string) = UUID32 input |> Some

    static member convert guid = guid |> createUUID 32 |> UUID32

    static member createNew() =
        Guid.CreateVersion7() |> createUUID 32 |> UUID32
