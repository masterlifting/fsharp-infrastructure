[<RequireQualifiedAccess>]
module Infrastructure.Prelude.String

open System
open System.Security.Cryptography

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

let toDeterministicHash (value: string) =
    use sha256 = SHA256.Create()

    value
    |> Text.Encoding.UTF8.GetBytes
    |> sha256.ComputeHash
    |> BitConverter.ToString
    |> _.Replace("-", "").ToLower()

let toDeterministicHash' (sha256: SHA256) (value: string) =
    value
    |> Text.Encoding.UTF8.GetBytes
    |> sha256.ComputeHash
    |> BitConverter.ToString
    |> _.Replace("-", "").ToLower()
