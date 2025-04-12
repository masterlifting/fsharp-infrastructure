module Infrastructure.Configuration.Providers.Json.Provider

open Microsoft.Extensions.Configuration
open Infrastructure.Domain
open Infrastructure.Configuration.Domain
open Infrastructure.Configuration.Providers.Builder

let init (connection: Connection) =
    try
        let builder = ConfigurationBuilder()
        connection.Files
        |> Seq.fold (fun builder file -> builder |> Json.addFile file) builder
        |> _.Build()
        |> Ok
    with ex ->
        Error
        <| Operation {
            Message = ex.Message
            Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
        }
