[<RequireQualifiedAccess>]
module Infrastructure.Prelude.TimeSpan

open System

let print (value: TimeSpan) =
    let inline pluralize (count, unit) =
        sprintf "%d %s%s" count unit (if count = 1 then "" else "s") |> Some

    [
        value.Days, "day"
        value.Hours, "hour"
        value.Minutes, "minute"
        value.Seconds, "second"
    ]
    |> List.choose (fun (count, unit) ->
        match count > 0 with
        | true -> pluralize (count, unit)
        | false -> None)
    |> function
        | [] -> "0 seconds"
        | parts -> String.concat " " parts
