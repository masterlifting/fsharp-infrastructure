[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Map

let combine (map1: Map<'k, 'v>) (map2: Map<'k, 'v>) =
    map2 |> Map.fold (fun acc key value -> acc |> Map.add key value) map1

let removeKeys (keys: 'k list) (map: Map<'k, 'v>) =
    keys |> List.fold (fun acc key -> acc |> Map.remove key) map

let reverse (map: Map<'k, 'v>) =
    map
    |> Map.fold
        (fun (acc: Map<'v, 'k list>) key value ->
            let key' = value
            let value' = key :: (acc |> Map.tryFind key' |> Option.defaultValue [])
            Map.add key' value' acc)
        Map.empty
