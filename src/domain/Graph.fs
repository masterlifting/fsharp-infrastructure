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

    member this.Id = this.Value.Id
    member this.ShortId = DELIMITER |> this.Id.Value.Split |> Array.last |> NodeIdValue

    member this.IdParts =
        DELIMITER |> this.Id.Value.Split |> List.ofArray |> List.map NodeIdValue

    member this.Name = this.Value.Name
    member this.ShortName = DELIMITER |> this.Name.Split |> Array.last
    member this.NameParts = DELIMITER |> this.Name.Split |> List.ofArray

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
