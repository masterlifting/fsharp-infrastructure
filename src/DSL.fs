[<AutoOpen>]
module Infrastructure.DSL

open System

[<RequireQualifiedAccess>]
module AP =
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

    let (|IsTimeSpan|_|) (input: string) =
        match TimeSpan.TryParse input with
        | true, value -> Some value
        | _ -> None

    let (|IsLettersOrNumbers|_|) (input: string) =
        match Text.RegularExpressions.Regex.IsMatch(input, "^[a-zA-Z0-9]+$") with
        | true -> Some input
        | _ -> None

[<AutoOpen>]
module Threading =
    open System.Threading
    let canceled (cToken: CancellationToken) = cToken.IsCancellationRequested
    let notCanceled (cToken: CancellationToken) = not <| cToken.IsCancellationRequested

[<RequireQualifiedAccess>]
module Seq =
    /// <summary>
    /// Reduces a sequence of results into a single result. If any of the results is an error, the whole result is an error.
    /// </summary>
    let roe data =
        let map state itemRes =
            state
            |> Result.bind (fun items -> itemRes |> Result.map (fun item -> item :: items))

        Seq.fold map (Ok []) data |> Result.map List.rev

    /// <summary>
    /// Reduces a sequence of results into a single result of items or errors
    /// </summary>
    let roes data =
        let map (items, errors) itemRes =
            match itemRes with
            | Ok item -> item :: items, errors
            | Error error -> items, error :: errors

        match Seq.fold map ([], []) data with
        | items, [] -> Ok items
        | _, errors -> Error errors

[<RequireQualifiedAccess>]
module Map =
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

[<RequireQualifiedAccess>]
module Graph =
    open Domain

    let buildNodeName parentName nodeName =
        match parentName with
        | None -> nodeName
        | Some parentName -> $"%s{parentName}.%s{nodeName}"

    let findNode<'a when 'a :> Graph.INodeName> nodeName (node: Graph.Node<'a>) =

        let rec innerLoop targetName nodeName (node: Graph.Node<'a>) =
            let nodeValue, nodeChildren = node.Deconstructed

            let nodeName = nodeName |> buildNodeName <| nodeValue.Name

            match nodeName = targetName with
            | true -> Some node
            | _ -> nodeChildren |> List.tryPick (innerLoop targetName (Some nodeName))

        innerLoop nodeName None node

[<RequireQualifiedAccess>]
module ResultAsync =
    let wrap f =
        function
        | Ok x -> f x
        | Error e -> async { return Error e }

    let bind f asyncResult =
        async {
            let! result = asyncResult
            return Result.bind f result
        }

    let bind' f asyncResult =
        async {
            match! asyncResult with
            | Ok result -> return! f result
            | Error err -> return Error err
        }

    let map f asyncResult =
        async {
            let! result = asyncResult
            return Result.map f result
        }

    let map' f asyncResult =
        async {
            match! asyncResult with
            | Ok result -> return Ok <| f result
            | Error err -> return Error err
        }

    let mapError f result =
        async { return Result.mapError f result }

    let mapError' f asyncResult =
        async {
            let! result = asyncResult
            return Result.mapError f result
        }

[<RequireQualifiedAccess>]
module Exception =
    let toMessage (ex: exn) =
        ex.InnerException
        |> Option.ofObj
        |> Option.map (_.Message)
        |> Option.defaultValue ex.Message

[<AutoOpen>]
module CE =
    type ResultAsyncBuilder() =
        member _.Bind(m, f) = ResultAsync.bind' f m
        member _.Return(m) = ResultAsync.map' id m

    let resultAsync = ResultAsyncBuilder()
