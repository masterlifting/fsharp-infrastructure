[<RequireQualifiedAccess>]
module Infrastructure.Configuration.Client

open System
open Microsoft.Extensions.Configuration
open Infrastructure.Domain
open Infrastructure.Prelude
open Infrastructure.Configuration.Providers

type Connection =
    | Yaml of Domain.Connection
    | Json of Domain.Connection

let init connection =
    match connection with
    | Connection.Json value -> value |> Json.Provider.init
    | Connection.Yaml value -> value |> Yaml.Provider.init

let tryGetSection<'a> sectionName (configuration: IConfigurationRoot) =
    configuration.GetSection(sectionName)
    |> Option.ofObj
    |> Option.bind (fun section ->
        match section.Exists() with
        | true -> section |> Parser.parse<'a> sectionName |> Some
        | false -> None)

let private getEnv key =
    try
        Ok
        <| match Environment.GetEnvironmentVariable(key) with
           | AP.IsString value -> Some value
           | _ -> None
    with ex ->
        Error <| NotFound ex.Message

let tryGetEnv key (configuration: IConfigurationRoot option) =
    match configuration with
    | Some config ->
        match config |> tryGetSection<string> key with
        | Some value -> Ok <| Some value
        | None -> getEnv key
    | None -> getEnv key
