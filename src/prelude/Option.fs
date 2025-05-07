[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Option

let wrap f =
    function
    | Some x -> f x
    | None -> async { return None }

let toResult f value =
    Option.map (f >> Result.map Some) value |> Option.defaultValue (Ok None)

let min<'a when 'a: comparison> (x: 'a option) (y: 'a option) =
    match x, y with
    | Some x, Some y -> Some(min x y)
    | Some x, None -> Some x
    | None, Some y -> Some y
    | None, None -> None

let max<'a when 'a: comparison> (x: 'a option) (y: 'a option) =
    match x, y with
    | Some x, Some y -> Some(max x y)
    | Some x, None -> Some x
    | None, Some y -> Some y
    | None, None -> None
