[<RequireQualifiedAccess>]
module Infrastructure.Prelude.TimeSpan

open System

let print (value: TimeSpan) =
    [ value.Days, "d"; value.Hours, "h"; value.Minutes, "m"; value.Seconds, "s" ]
    |> List.choose (fun (count, unit) ->
        match count > 0 with
        | true -> $"%d{count}%s{unit}" |> Some
        | false -> None)
    |> function
        | [] -> "0s"
        | parts -> String.concat " " parts
