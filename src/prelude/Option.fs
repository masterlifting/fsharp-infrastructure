[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Option

let wrap f =
    function
    | Some x -> f x
    | None -> async { return None }

let toResult f value =
    Option.map (f >> Result.map Some) value |> Option.defaultValue (Ok None)
