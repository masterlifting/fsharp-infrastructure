[<RequireQualifiedAccess>]
module Infrastructure.Logging.Client

open Infrastructure
open Infrastructure.Logging.Domain
open Infrastructure.Logging.Providers

type Provider =
    | Console of Level
    | File of Level

let setLevel cfg =
    match cfg with
    | Some cfg ->
        cfg
        |> Configuration.Client.getValue<string> "LOG_LEVEL"
        |> Option.map Builder.parseLevel
        |> Option.defaultValue Information
    | None ->
        match Configuration.Client.getEnvValue "LOG_LEVEL" with
        | Ok(Some value) -> value |> Builder.parseLevel
        | Ok None -> Information
        | Error _ -> Information

let init provider =
    match provider with
    | Console level -> Console.Provider.init level
    | File level -> File.Provider.init level
