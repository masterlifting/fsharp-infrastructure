[<RequireQualifiedAccess>]
module Infrastructure.Domain.Graph

open System
open Infrastructure.Domain

[<Literal>]
let internal DELIMITER = "."

/// <summary>
/// Represents a unique identifier for a node in a graph.
/// </summary>
/// <remarks>
/// The value of a NodeId is a string that split into parts using a delimiter.
/// </remarks>
type NodeId =
    | NodeIdValue of string

    member this.Value =
        match this with
        | NodeIdValue id -> id

    static member create(value: string) =
        match value |> String.IsNullOrWhiteSpace with
        | false -> NodeIdValue value |> Ok
        | true -> $"NodeId value: {value}" |> NotSupported |> Error

    static member New = Guid.NewGuid() |> string |> NodeIdValue

    member this.Split() =
        DELIMITER |> this.Value.Split |> List.ofArray

    /// <summary>
    /// Gets the part of the NodeId value at the specified index.
    /// </summary>
    /// <param name="index">
    /// The index of the part to get.
    /// </param>
    member this.TryGetPart index =
        let parts = this.Split()

        match parts.Length > index with
        | true -> Some(parts[index] |> NodeIdValue)
        | false -> None

    member this.TryReplacePart index value =
        let parts = this.Split()

        match parts.Length > index with
        | true ->
            parts
            |> List.mapi (fun i part ->
                match i = index with
                | true -> value
                | false -> part)
            |> String.concat DELIMITER
            |> NodeIdValue
            |> Ok
        | false ->
            $"Index: {index} is out of range for NodeId parts: {this.Value}"
            |> NotFound
            |> Error

    member private this.TryTakeRangeValues (startIndex: int) (length: int option) =
        let values = this.Split()

        match values.Length >= startIndex with
        | false -> None
        | true ->
            match length with
            | None -> Some(values[startIndex..])
            | Some len ->
                match values.Length >= startIndex + len with
                | true -> Some(values[startIndex .. startIndex + len - 1])
                | false -> None

    member this.TryTakeRange (startIndex: int) (length: int option) =
        this.TryTakeRangeValues startIndex length |> Option.map (List.map NodeIdValue)

    member this.TryTake (startIndex: int) (length: int option) =
        this.TryTakeRange startIndex length
        |> Option.map (List.map _.Value)
        |> Option.map (String.concat DELIMITER)
        |> Option.map NodeIdValue

    /// <summary>
    /// Determines whether the NodeId or any part of its value is contained in the specified id.
    /// </summary>
    /// <param name="id"> The id to check for containment. </param>
    member this.Contains(id: NodeId) = this.Value.Contains id.Value

    /// <summary>
    /// Determines whether the NodeId or any part of its value is contained in one of the specified ids.
    /// </summary>
    /// <param name="ids">The ids to check for containment.</param>
    member this.In(ids: NodeId list) = ids |> List.exists this.Contains

/// <summary>
/// Represents a node in a graph.
/// </summary>
type INode =
    
    /// <summary>
    /// Gets the unique identifier of the node.
    /// </summary>
    /// <remarks>
    /// The identifier is unique within the graph that split into parts using a delimiter.
    /// </remarks>
    abstract member Id: NodeId
    
    /// <summary>
    /// Gets the name of the node.
    /// </summary>
    /// <remarks>
    /// The name is unique within the graph that split into parts using a delimiter.
    /// </remarks>
    abstract member Name: string
    abstract member set: NodeId * string -> INode

type Node<'a when 'a :> INode> =
    | Node of 'a * Node<'a> list

    member this.Value =
        match this with
        | Node(current, _) -> current

    member this.Id = this.Value.Id
    member this.ShortId = DELIMITER |> this.Id.Value.Split |> Array.last |> NodeIdValue

    member this.Name = this.Value.Name
    member this.ShortName = DELIMITER |> this.Name.Split |> Array.last

    member private this.GetChildren(id: NodeId, name) =
        match this with
        | Node(_, children) ->
            children
            |> List.map (fun node ->
                let id = [ id.Value; node.Id.Value ] |> String.concat DELIMITER |> NodeIdValue
                let name = [ name; node.Name ] |> String.concat DELIMITER

                let value = (id, name) |> node.Value.set :?> 'a

                let children =
                    match node with
                    | Node(_, children) -> children

                Node(value, children))

    member this.Children = (this.Id, this.Name) |> this.GetChildren
