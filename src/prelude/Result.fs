[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Result

/// <summary>
/// Reduces a sequence of results into a single result. If any of the results is an error, the whole result is an error.
/// </summary>
let choose data =
    let map state itemRes =
        state
        |> Result.bind (fun items -> itemRes |> Result.map (fun item -> item :: items))

    Seq.fold map (Ok []) data |> Result.map List.rev

/// <summary>
/// Unzips a sequence of results into two sequences: one for the items and one for the errors.
/// </summary>
let unzip data =
    let map itemRes =
        match itemRes with
        | Ok item -> Some item, None
        | Error error -> None, Some error

    let choose (items, errors) =
        let items = items |> Seq.choose id |> Seq.toList
        let errors = errors |> Seq.choose id |> Seq.toList
        items, errors

    data |> Seq.map map |> Seq.unzip |> choose
