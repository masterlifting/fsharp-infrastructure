[<RequireQualifiedAccess>]
module Infrastructure.Configuration.Client

open System
open Microsoft.Extensions.Configuration
open Infrastructure.Domain
open Infrastructure.Prelude
open Infrastructure.Configuration.Providers
open Infrastructure.Configuration.Providers.Domain

type Provider =
    | Yaml of Yaml.Client
    | Json of Json.Client

type Connection =
    | Yaml of Yaml.Connection
    | Json of Json.Connection

let init connection =
    match connection with
    | Connection.Json value -> value |> Json.Provider.init |> Result.map Provider.Json
    | Connection.Yaml value -> value |> Yaml.Provider.init |> Result.map Provider.Yaml

let getSection<'a> sectionName (configuration: IConfigurationRoot) =
    configuration.GetSection(sectionName)
    |> Option.ofObj
    |> Option.bind (fun section ->
        match section.Exists() with
        | true -> section |> get<'a> sectionName |> Some
        | false -> None)

/// <summary>
/// Get environment variable from environment variables.
/// </summary>
/// <param name="key"> Environment variable key </param>
/// <returns> Monad with environment variable value as a string </returns>
let tryGetEnv key =
    try
        Ok
        <| match Environment.GetEnvironmentVariable(key) with
           | AP.IsString value -> Some value
           | _ -> None
    with ex ->
        Error <| NotFound ex.Message

/// <summary>
/// Get environment variable from a configuration file first, then from environment variables.
/// </summary>
/// <param name="key"> Environment variable key </param>
/// <param name="configuration"> IConfigurationRoot </param>
/// <returns> Monad with environment variable value as a string </returns>
let tryGetEnv key configuration =
    match configuration |> getSection<string> key with
    | Some value -> Ok <| Some value
    | None -> tryGetEnv key
