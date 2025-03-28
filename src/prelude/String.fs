[<RequireQualifiedAccess>]
module Infrastructure.Prelude.String

open System
open System.Security.Cryptography
open Infrastructure.Domain

let fromTimeSpan (value: TimeSpan) =
    let format = "dd\\.hh\\:mm\\:ss"
    value.ToString(format)

let fromDateTime (value: DateTime) =
    let format = "yyyy-MM-dd HH:mm:ss"
    value.ToString(format)

let addLines count =
    Seq.init count (fun _ -> Environment.NewLine) |> String.concat ""

let toDefault (value: string | null) =
    match value with
    | null -> String.Empty
    | v -> v

let toHash (value: string) =
    fun (algorithm: HashAlgorithm) ->
        try
            value
            |> Text.Encoding.UTF8.GetBytes
            |> algorithm.ComputeHash
            |> BitConverter.ToString
            |> _.Replace("-", "").ToLower()
            |> Ok
        with ex ->
            Error
            <| Operation {
                Message = $"The hash calculation caused the error: {ex |> Exception.toMessage}"
                Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
            }

let toSpan (value: string) = value.AsSpan()
