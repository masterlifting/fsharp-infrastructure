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
    open Domain
    open System.Threading

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

    /// <summary>
    /// Executes a sequence of asynchronous operations in parallel or sequentially.
    /// </summary>
    /// <param name="name">The name of the parent node.</param>
    /// <param name="collection">The collection of the nodes.</param>
    /// <param name="handle">The handler of the node.</param>
    /// <returns>The asynchronous operation.</returns>
    let rec parallelOrSequential name (collection: #IParallelOrSequential list) handle =
        async {
            if collection.Length > 0 then
                match collection |> List.takeWhile (fun step -> step.IsParallel) with
                | parallelItemHead :: parallelItemsTail when parallelItemsTail.Length = 0 ->

                    let sequentialItems =
                        collection |> List.skip 1 |> List.takeWhile (fun step -> not step.IsParallel)

                    do!
                        [ parallelItemHead ] @ sequentialItems
                        |> List.map (fun item ->
                            let name =
                                let parentName = Option.defaultValue "" name
                                let itemName = Option.defaultValue "" item.Name

                                match parentName with
                                | "" -> itemName
                                | _ -> $"{parentName}.{itemName}"

                            handle name item)
                        |> Async.Sequential
                        |> Async.Ignore

                | parallelItems ->

                    do!
                        parallelItems
                        |> List.map (fun item ->
                            let name =
                                let parentName = Option.defaultValue "" name
                                let itemName = Option.defaultValue "" item.Name

                                match parentName with
                                | "" -> itemName
                                | _ -> $"{parentName}.{itemName}"

                            handle name item)
                        |> Async.Parallel
                        |> Async.Ignore
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
