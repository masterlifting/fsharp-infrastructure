[<RequireQualifiedAccess>]
module Infrastructure.Prelude.AP

open System

let (|IsString|_|) (input: string) =
    match String.IsNullOrWhiteSpace input with
    | false -> Some input
    | _ -> None

let (|IsInt|_|) (input: string) =
    match Int32.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsFloat|_|) (input: string) =
    match Double.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsGuid|_|) (input: string) =
    match Guid.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsTimeSpan|_|) (input: string) =
    match TimeSpan.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsDateOnly|_|) (input: string) =
    match DateOnly.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsTimeOnly|_|) (input: string) =
    match TimeOnly.TryParse input with
    | true, value -> Some value
    | _ -> None

let (|IsLettersOrNumbers|_|) (input: string) =
    match Text.RegularExpressions.Regex.IsMatch(input, "^[a-zA-Z0-9]+$") with
    | true -> Some input
    | _ -> None
