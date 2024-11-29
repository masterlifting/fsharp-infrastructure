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

[<AutoOpen>]
module Threading =
    open System.Threading
    let canceled (cToken: CancellationToken) = cToken.IsCancellationRequested
    let notCanceled (cToken: CancellationToken) = not <| cToken.IsCancellationRequested

[<RequireQualifiedAccess>]
module Seq =

    let unzip tuples =
        let map (acc1, acc2) (item1, item2) = (item1 :: acc1, item2 :: acc2)

        tuples
        |> Seq.fold map ([], [])
        |> fun (acc1, acc2) -> List.rev acc1, List.rev acc2

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
module Result =

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

[<RequireQualifiedAccess>]
module Graph =
    open Domain

    let buildNodeName nodeName parentName =
        match parentName with
        | None -> nodeName
        | Some parentName -> $"%s{parentName}%s{Graph.DELIMITER}%s{nodeName}"

    let buildNodeNameOfSeq data = data |> String.concat Graph.DELIMITER

    let splitNodeName (name: string) =
        Graph.DELIMITER |> name.Split |> Array.toList

    module DFS =

        /// <summary>
        /// Tries to find a node by its full name in the graph using depth-first search.
        /// </summary>
        /// <param name="name">The full name of the node.</param>
        /// <param name="graph">The graph to search in.</param>
        /// <returns>The node if found, otherwise None.</returns>
        let rec tryFindByName<'a when 'a :> Graph.INodeName> name (graph: Graph.Node<'a>) =
            match graph.FullName = name with
            | true -> Some graph
            | false -> graph.Children |> List.tryPick (tryFindByName name)

        /// <summary>
        /// Tries to find a node by its ID in the graph using depth-first search.
        /// </summary>
        /// <param name="nodeId">The ID of the node.</param>
        /// <param name="graph">The graph to search in.</param>
        /// <returns>The node if found, otherwise None.</returns>
        let rec tryFindById<'a when 'a :> Graph.INodeName> nodeId (graph: Graph.Node<'a>) =
            match graph.FullId = nodeId with
            | true -> Some graph
            | false -> graph.Children |> List.tryPick (tryFindById nodeId)

    module BFS =

        /// <summary>
        /// Tries to find a node by its full name in the graph using breadth-first search.
        /// </summary>
        /// <param name="name">The full name of the node.</param>
        /// <param name="graph">The graph to search in.</param>
        /// <returns>The node if found, otherwise None.</returns>
        let tryFindByName<'a when 'a :> Graph.INodeName> name graph =
            let rec search (nodes: Graph.Node<'a> list) =
                match nodes with
                | [] -> None
                | node :: tail ->
                    match node.FullName = name with
                    | true -> Some node
                    | false -> search (tail @ node.Children)

            [ graph ] |> search

        /// <summary>
        /// Tries to find a node by its ID in the graph using breadth-first search.
        /// </summary>
        /// <param name="nodeId">The ID of the node.</param>
        /// <param name="graph">The graph to search in.</param>
        /// <returns>The node if found, otherwise None.</returns>
        let tryFindById<'a when 'a :> Graph.INodeName> nodeId graph =
            let rec search (nodes: Graph.Node<'a> list) =
                match nodes with
                | [] -> None
                | node :: tail ->
                    match node.FullId = nodeId with
                    | true -> Some node
                    | false -> search (tail @ node.Children)

            [ graph ] |> search

[<RequireQualifiedAccess>]
module Async =

    let bind next asyncWorkflow =
        async {
            let! result = asyncWorkflow
            return! next result
        }

    let map next asyncWorkflow =
        async {
            let! result = asyncWorkflow
            return next result
        }

[<RequireQualifiedAccess>]
module Option =

    let toResult f value =
        Option.map (f >> Result.map Some) value |> Option.defaultValue (Ok None)

[<RequireQualifiedAccess>]
module OptionAsync =

    let wrap f =
        function
        | Some x -> f x
        | None -> async { return None }

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

    let bindAsync f asyncResult =
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

    let mapAsync f asyncResult =
        async {
            match! asyncResult with
            | Ok result -> return Ok <| f result
            | Error err -> return Error err
        }

    let mapError f asyncResult =
        async {
            let! result = asyncResult
            return Result.mapError f result
        }

    let mapErrorAsync f asyncResult =
        async {
            match! asyncResult with
            | Ok result -> return Ok result
            | Error err ->
                let! err = f err
                return Error err
        }

[<RequireQualifiedAccess>]
module Exception =
    let toMessage (ex: exn) =
        ex.InnerException
        |> Option.ofObj
        |> Option.map _.Message
        |> Option.defaultValue ex.Message

[<AutoOpen>]
module String =
    let fromTimeSpan (value: TimeSpan) =
        let format = "dd\\.hh\\:mm\\:ss"
        value.ToString(format)

    let fromDateTime (value: DateTime) =
        let format = "yyyy-MM-dd HH:mm:ss"
        value.ToString(format)

[<AutoOpen>]
module CE =
    type ResultBuilder() =
        member _.Bind(x, f) = Result.bind f x
        member _.Return x = Ok x
        member _.ReturnFrom x = x

    type ResultAsyncBuilder() =
        member _.Bind(m, f) = ResultAsync.bindAsync f m
        member _.Return m = ResultAsync.mapAsync id m
        member _.ReturnFrom m = ResultAsync.wrap id m
