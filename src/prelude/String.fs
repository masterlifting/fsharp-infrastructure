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

let encrypt (key: string) (plaintext: string) =
    try
        let keyBytes = SHA256.HashData(Encoding.UTF8.GetBytes(key))
        use aes = new AesGcm(keyBytes, 16) // Specify tag size of 16 bytes
        let nonce = RandomNumberGenerator.GetBytes(12)
        let plaintextBytes = Encoding.UTF8.GetBytes(plaintext)
        let ciphertext = Array.zeroCreate<byte> plaintextBytes.Length
        let tag = Array.zeroCreate<byte> 16
        aes.Encrypt(nonce, plaintextBytes, ciphertext, tag)
        let combined = Array.concat [nonce; tag; ciphertext]
        Convert.ToBase64String(combined) |> Ok
    with ex ->
        Error
        <| Operation {
            Message = $"Encryption failed: {ex |> Exception.toMessage}"
            Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
        }

let decrypt (key: string) (encrypted: string) =
    try
        let keyBytes = SHA256.HashData(Encoding.UTF8.GetBytes(key))
        use aes = new AesGcm(keyBytes, 16) // Specify tag size of 16 bytes
        let combined = Convert.FromBase64String(encrypted)
        let nonce = combined.[..11]
        let tag = combined.[12..27]
        let ciphertext = combined.[28..]
        let plaintextBytes = Array.zeroCreate<byte> ciphertext.Length
        aes.Decrypt(nonce, ciphertext, tag, plaintextBytes)
        Encoding.UTF8.GetString(plaintextBytes) |> Ok
    with ex ->
        Error
        <| Operation {
            Message = $"Decryption failed: {ex |> Exception.toMessage}"
            Code = (__SOURCE_DIRECTORY__, __SOURCE_FILE__, __LINE__) |> Line |> Some
        }
