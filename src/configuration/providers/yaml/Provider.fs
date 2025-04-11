module Infrastructure.Configuration.Providers.Yaml.Provider

open System
open System.Collections.Concurrent
open Infrastructure.Domain
open Infrastructure.Configuration.Providers.Domain.Yaml


let init connection =
    try
        Ok
        <| {
               Provider = 
           }
    with ex ->
        Error
        <| Operation {
            Message = ex.Message
            Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
