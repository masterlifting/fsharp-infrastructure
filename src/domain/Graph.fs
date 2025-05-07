[<RequireQualifiedAccess>]
module Infrastructure.Domain.Graph

open System
open Infrastructure.Domain

[<Literal>]
let private DELIMITER = "."

type NodeId =
    | NodeIdValue of string

    member this.Value =
        match this with
        | NodeIdValue id -> id

    static member parse(value: string) =
        match value |> String.IsNullOrWhiteSpace with
        | false -> NodeIdValue value |> Ok
        | true -> $"NodeId value '{value}' is not supported." |> NotSupported |> Error

    static member createNew() = Guid.NewGuid() |> string |> NodeIdValue

    static member combine(nodeIds: NodeId seq) =
        nodeIds |> Seq.map _.Value |> String.concat DELIMITER |> NodeIdValue

    static member splitValues(id: NodeId) =
        DELIMITER |> id.Value.Split |> List.ofArray

    static member split(id: NodeId) =
        id |> NodeId.splitValues |> Seq.map NodeIdValue

    member this.IsIn(id: NodeId) = this.Value.Contains id.Value

    member this.IsInSeq(ids: NodeId seq) =
        ids |> Seq.exists (fun id -> id.IsIn this)

/// <summary>
/// Represents a node in a graph.
/// </summary>
type INode =
    abstract member Id: NodeId
    abstract member set: NodeId -> INode

type Node<'a when 'a :> INode> =
    | Node of 'a * Node<'a> list

    member this.Value =
        match this with
        | Node(current, _) -> current

    member this.Id = this.Value.Id
    member this.ShortId = DELIMITER |> this.Id.Value.Split |> Array.last |> NodeIdValue

    member private this.GetChildren(id: NodeId) =
        match this with
        | Node(_, children) ->
            children
            |> List.map (fun node ->
                let id = [ id.Value; node.Id.Value ] |> String.concat DELIMITER |> NodeIdValue

                let value = id |> node.Value.set :?> 'a

                let children =
                    match node with
                    | Node(_, children) -> children

                Node(value, children))

    member this.Children = this.Id |> this.GetChildren
