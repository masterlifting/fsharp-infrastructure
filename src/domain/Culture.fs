[<AutoOpen>]
module Infrastructure.Domain.Culture

type Culture =
    | English
    | Russian
    | Chinese
    | Spanish
    | Hindi
    | Arabic
    | Serbian
    | Portuguese
    | French
    | German
    | Japanese
    | Korean

    static member parse value =
        match value with
        | "RU" -> Russian
        | "ZH" -> Chinese
        | "ES" -> Spanish
        | "HI" -> Hindi
        | "AR" -> Arabic
        | "SB" -> Serbian
        | "PT" -> Portuguese
        | "FR" -> French
        | "DE" -> German
        | "JA" -> Japanese
        | "KO" -> Korean
        | _ -> English

    member this.Code =
        match this with
        | English -> "EN"
        | Russian -> "RU"
        | Chinese -> "ZH"
        | Spanish -> "ES"
        | Hindi -> "HI"
        | Arabic -> "AR"
        | Serbian -> "SB"
        | Portuguese -> "PT"
        | French -> "FR"
        | German -> "DE"
        | Japanese -> "JA"
        | Korean -> "KO"

    member this.Name =
        match this with
        | English -> "English"
        | Russian -> "Russian"
        | Chinese -> "Chinese"
        | Spanish -> "Spanish"
        | Hindi -> "Hindi"
        | Arabic -> "Arabic"
        | Serbian -> "Serbian"
        | Portuguese -> "Portuguese"
        | French -> "French"
        | German -> "German"
        | Japanese -> "Japanese"
        | Korean -> "Korean"

    static member createDefault() = English
