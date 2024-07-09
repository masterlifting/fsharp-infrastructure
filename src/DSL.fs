module Infrastructure.DSL

open Infrastructure.Domain.Errors

open System

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

module Threading =
    open System.Threading
    let canceled (cToken: CancellationToken) = cToken.IsCancellationRequested
    let notCanceled (cToken: CancellationToken) = not <| cToken.IsCancellationRequested

[<RequireQualifiedAccessAttribute>]
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

[<RequireQualifiedAccessAttribute>]
module Graph =
    open Domain.Graph

    let buildNodeName parentName nodeName =
        match parentName with
        | None -> nodeName
        | Some parentName -> $"{parentName}.{nodeName}"

    let findNode<'a when 'a :> INodeName> nodeName (node: Node<'a>) =

        let rec innerLoop targetName nodeName (node: Node<'a>) =
            let nodeValue, nodeChildren = node.Deconstructed

            let nodeName = nodeName |> buildNodeName <| nodeValue.Name

            match nodeName = targetName with
            | true -> Some node
            | _ -> nodeChildren |> List.tryPick (innerLoop targetName (Some nodeName))

        innerLoop nodeName None node

module SerDe =
    [<RequireQualifiedAccessAttribute>]
    module Json =
        open System.Text.Json

        let private getWebApiOptions () =
            let options = JsonSerializerOptions()
            options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
            options

        let private getStandardOptions () =
            let options = JsonSerializerOptions()
            options

        type OptionType =
            | WebApi
            | Standard

        let serialize data =
            try
                Ok <| JsonSerializer.Serialize data
            with ex ->
                Error <| NotSupported ex.Message

        let serialize' optionsType data =

            let options =
                match optionsType with
                | OptionType.WebApi -> getWebApiOptions ()
                | OptionType.Standard -> getStandardOptions ()

            try
                Ok <| JsonSerializer.Serialize(data, options)
            with ex ->
                Error <| NotSupported ex.Message

        let deserialize<'a> (data: string) =
            try
                Ok <| JsonSerializer.Deserialize<'a> data
            with ex ->
                Error <| NotSupported ex.Message

        let deserialize'<'a> optionsType (data: string) =

            let options =
                match optionsType with
                | OptionType.WebApi -> getWebApiOptions ()
                | OptionType.Standard -> getStandardOptions ()

            try
                Ok <| JsonSerializer.Deserialize<'a>(data, options)
            with ex ->
                Error <| NotSupported ex.Message

    module Yaml =
        open YamlDotNet.Serialization

        let private serializer = SerializerBuilder().Build()
        let private deserializer = DeserializerBuilder().Build()

        let serialize data =
            try
                Ok <| serializer.Serialize data
            with ex ->
                Error <| NotSupported ex.Message

        let deserialize<'a> (data: string) =
            try
                Ok <| deserializer.Deserialize<'a> data
            with ex ->
                Error <| NotSupported ex.Message

[<RequireQualifiedAccessAttribute>]
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

module CE =
    type ResultAsyncBuilder() =
        member _.Bind(m, f) = ResultAsync.bind' f m
        member _.Return(m) = ResultAsync.map' id m

    let resultAsync = ResultAsyncBuilder()

//module private CETest =
    // type WorkflowBuilder() =
    //     member _.Bind(m, f) = Option.bind f m
    //     member _.Return(m) = Some m

    // let strToInt (s: string) =
    //     match Int32.TryParse s with
    //     | true, i -> Some i
    //     | _ -> None

    // let maybe = WorkflowBuilder()

    // let strWorkflow (data: string array) =
    //     maybe {

    //         let! a = strToInt data[0]
    //         let! b = strToInt data[1]
    //         let! c = strToInt data[2]
    //         return a + b + c
    //     }

    // let good = strWorkflow [| "1"; "2"; "3" |]
    // let bad = strWorkflow [| "1"; "a"; "2" |]


    //let private (>>=) m f = Option.bind f m

    //let strAdd str i =
    //    match strToInt str with
    //    | Some x -> Some(x + i)
    //    | None -> None

    // let good' = strToInt "1" >>= strAdd "2" >>= strAdd "3"
    // let bad' = strToInt "1" >>= strAdd "a" >>= strAdd "2"
