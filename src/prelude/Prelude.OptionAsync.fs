[<RequireQualifiedAccess>]
module Infrastructure.Prelude.OptionAsync

let toResult f value =
    Option.map (f >> Result.map Some) value |> Option.defaultValue (Ok None)
