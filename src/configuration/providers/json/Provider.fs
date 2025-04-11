module Infrastructure.Configuration.Providers.Json.Provider

open System
open System.Collections.Concurrent
open Infrastructure.Domain
open Infrastructure.Configuration.Providers.Domain.Json


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
