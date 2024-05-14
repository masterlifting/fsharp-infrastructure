module Infrastructure.DSL

open System

module AP =
    let (|IsString|_|) (input: string) =
        match not <| String.IsNullOrWhiteSpace input with
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

module Seq =

    let resultOrError collection =
        let checkItemResult state itemResult =
            match state with
            | Error error -> Error error
            | Ok items ->
                match itemResult with
                | Error error -> Error error
                | Ok item -> Ok <| item :: items

        match Seq.fold checkItemResult (Ok []) collection with
        | Error error -> Error error
        | Ok items -> Ok <| List.rev items

module Graph =
    open Domain

    let buildNodeName parentName nodeName =
        let parentName = Option.defaultValue "" parentName

        match parentName with
        | "" -> nodeName
        | _ -> $"{parentName}.{nodeName}"

    let findNode<'a when 'a :> IGraphNodeName> nodeName (graph: Graph<'a>) =

        let rec innerLoop targetName nodeName (graph: Graph<'a>) =
            match graph with
            | Graph(node, children) ->
                let nodeName = nodeName |> buildNodeName <| node.Name

                if nodeName = targetName then
                    Some node
                else
                    children |> List.choose (innerLoop targetName (Some nodeName)) |> List.tryHead

        innerLoop nodeName None graph

    let findNode'<'a when 'a :> IGraphNodeName> nodeName (graphs: Graph<'a> list) =
        graphs |> List.choose (findNode nodeName) |> List.tryHead

    let rec doParallelOrSequential'<'a when 'a :> IGraphNodeHandle> nodeName (nodes: Graph<'a> list) handleNode =

        let inline handle (graph: Graph<'a>) =
            async {
                let node, nodes = graph.Deconstructed

                let nodeName = nodeName |> buildNodeName <| node.Name
                do! handleNode nodeName node
                do! doParallelOrSequential' (Some nodeName) nodes handleNode
            }

        async {
            if nodes.Length > 0 then
                let task, skipLength =

                    let parallelNodes = nodes |> List.takeWhile (fun node -> node.Current.IsParallel)

                    match parallelNodes with
                    | parallelNodes when parallelNodes.Length < 2 ->

                        let sequentialNodes =
                            nodes |> List.skip 1 |> List.takeWhile (fun node -> not node.Current.IsParallel)

                        let task = [ nodes[0] ] @ sequentialNodes |> List.map handle |> Async.Sequential
                        (task, sequentialNodes.Length + 1)

                    | parallelNodes ->
                        let task = parallelNodes |> List.map handle |> Async.Parallel
                        (task, parallelNodes.Length)

                do! task |> Async.Ignore
                do! doParallelOrSequential' nodeName (nodes |> List.skip skipLength) handleNode
        }

    let rec doParallelOrSequential<'a when 'a :> IGraphNodeHandle> (graph: Graph<'a>) handleNode =
        let node, nodes = graph.Deconstructed
        let nodeName = node.Name

        async {
            do! handleNode nodeName node
            do! doParallelOrSequential' (Some nodeName) nodes handleNode
        }

module SerDe =
    module Json =
        open System.Text.Json

        let serialize data =
            try
                Ok <| JsonSerializer.Serialize data
            with ex ->
                Error ex.Message

        let deserialize<'a> (data: string) =
            try
                Ok <| JsonSerializer.Deserialize<'a> data
            with ex ->
                Error ex.Message

module private CETest =
    type WorkflowBuilder() =
        member _.Bind(m, f) = Option.bind f m
        member _.Return(m) = Some m

    let strToInt (s: string) =
        match System.Int32.TryParse s with
        | true, i -> Some i
        | _ -> None

    let maybe = new WorkflowBuilder()

    let strWorkflow (data: string array) =
        maybe {

            let! a = strToInt data.[0]
            printfn "a: %d" a
            let! b = strToInt data.[1]
            let! c = strToInt data.[2]
            return a + b + c
        }

    let good = strWorkflow [| "1"; "2"; "3" |]
    let bad = strWorkflow [| "1"; "a"; "2" |]


    let private (>>=) m f = Option.bind f m

    let strAdd str i =
        match strToInt str with
        | Some x -> Some(x + i)
        | None -> None

    let good' = strToInt "1" >>= strAdd "2" >>= strAdd "3"
    let bad' = strToInt "1" >>= strAdd "a" >>= strAdd "2"
