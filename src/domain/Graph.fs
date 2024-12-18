[<RequireQualifiedAccess>]
module Infrastructure.Domain.Graph

open System
open Infrastructure.Domain

[<Literal>]
let internal DELIMITER = "."

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

type INodeName =
    abstract member Id: NodeId
    abstract member Name: string
    abstract member set: NodeId * string -> INodeName

type Node<'a when 'a :> INodeName> =
    | Node of 'a * Node<'a> list

    member this.Value =
        match this with
        | Node(current, _) -> current

    member this.FullId = this.Value.Id
    member this.ShortId = DELIMITER |> this.FullId.Value.Split |> Array.last |> NodeIdValue
    member this.FullIds = DELIMITER |> this.FullId.Value.Split |> List.ofArray |> List.map NodeIdValue

    member this.FullName = this.Value.Name
    member this.ShortName = DELIMITER |> this.FullName.Split |> Array.last
    member this.FullNames = DELIMITER |> this.FullName.Split |> List.ofArray

    member private this.GetChildren(id: NodeId, name) =
        match this with
        | Node(_, children) ->
            children
            |> List.map (fun node ->
                let id = [ id.Value; node.FullId.Value ] |> String.concat DELIMITER |> NodeIdValue
                let name = [ name; node.FullName ] |> String.concat DELIMITER

                let value = (id, name) |> node.Value.set :?> 'a

                let children =
                    match node with
                    | Node(_, children) -> children

                Node(value, children))

    member this.Children = (this.FullId, this.FullName) |> this.GetChildren
