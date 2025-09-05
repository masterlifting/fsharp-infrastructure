[<RequireQualifiedAccess>]
module Infrastructure.Logging.Client

open Infrastructure
open Infrastructure.Logging.Domain
open Infrastructure.Logging.Providers

type Provider =
    | Console of Level
    | File of Level

let getLevel ()=
    match Configuration.Client.getEnv "LOG_LEVEL" with
    | Ok (Some value) -> value |> Builder.parseLevel
    | Ok None -> Information
    | Error _ -> Information

let init provider =
    match provider with
    | Console level -> Console.Provider.init level
    | File level -> File.Provider.init level