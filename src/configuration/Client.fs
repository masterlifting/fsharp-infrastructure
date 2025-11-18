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
    | Json value -> value |> Json.Provider.init
    | Yaml value -> value |> Yaml.Provider.init

let getValue<'a> key (cfg: IConfigurationRoot) =
    cfg.GetSection key
    |> fun section ->
        match section.Exists() with
        | true -> section |> Parser.parse<'a> key |> Some
        | false -> None

let getEnvValue key =
    try
        Ok
        <| match Environment.GetEnvironmentVariable key with
           | AP.IsString value -> Some value
           | _ -> None
    with ex ->
        Error <| NotFound ex.Message
