[<RequireQualifiedAccess>]
module Infrastructure.Logging.Client

open Infrastructure
open Infrastructure.Logging.Domain
open Infrastructure.Logging.Providers

type Provider =
    | Console of Level
    | File of Level

let init provider =
    match provider with
    | Console level -> Console.Provider.init level
    | File level -> File.Provider.init level

let tryFindLevel cfg =
    match cfg |> Configuration.Client.tryGetSection<string> CFG_DEFAULT_SECTION_NAME with
    | Some value -> value |> Builder.parseLevel
    | None -> Information
