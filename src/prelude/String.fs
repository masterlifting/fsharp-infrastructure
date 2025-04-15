[<RequireQualifiedAccess>]
module Infrastructure.Prelude.String

open System
open System.Security.Cryptography
open Infrastructure.Domain
open System.Text

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

let has (pattern: string) (value: string) =
    value.Contains(pattern, StringComparison.OrdinalIgnoreCase)

let toHash (value: string) =
    fun (algorithm: HashAlgorithm) ->
        try
            value
            |> Encoding.UTF8.GetBytes
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

let private generateSalt (key: int) = BitConverter.GetBytes(key) |> Array.rev

let encrypt (key: int) (value: string) =
    try
        let salt = generateSalt key
        let bytes = Encoding.UTF8.GetBytes(value)
        let encryptedBytes = bytes |> Array.mapi (fun i b -> b ^^^ salt[i % salt.Length])
        Convert.ToBase64String(encryptedBytes) |> Ok
    with ex ->
        Error
        <| Operation {
            Message = $"Encryption failed: {ex |> Exception.toMessage}"
            Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
        }

let decrypt (key: int) (value: string) =
    try
        let salt = generateSalt key
        let encryptedBytes = Convert.FromBase64String(value)
        let decryptedBytes =
            encryptedBytes |> Array.mapi (fun i b -> b ^^^ salt[i % salt.Length])
        Encoding.UTF8.GetString(decryptedBytes) |> Ok
    with ex ->
        Error
        <| Operation {
            Message = $"Decryption failed: {ex |> Exception.toMessage}"
            Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
        }
