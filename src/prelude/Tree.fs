[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Tree

open System
open Infrastructure.Domain

/// <summary>
/// Represents Depth-first search (DFS).
/// </summary>
module DFS =

    /// <summary>
    /// Tries to find a node by its ID in the tree using depth-first search.
    /// </summary>
    /// <param name="nodeId">Id of the node.</param>
    /// <param name="tree">The tree to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let tryFind<'a when 'a :> Tree.INode> nodeId (tree: Tree.Node<'a>) =
        let rec search (node: Tree.Node<'a>) =
            match node.Id = nodeId with
            | true -> Some node
            | false -> node.Children |> List.tryPick search

        tree |> search

/// <summary>
/// Represents breadth-first search (BFS).
/// </summary>
module BFS =

    /// <summary>
    /// Tries to find a node by its ID in the tree using breadth-first search.
    /// </summary>
    /// <param name="nodeId">Id of the node.</param>
    /// <param name="tree">The tree to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let tryFind<'a when 'a :> Tree.INode> nodeId tree =
        let rec search (nodes: Tree.Node<'a> list) =
            match nodes with
            | [] -> None
            | node :: tail ->
                match node.Id = nodeId with
                | true -> Some node
                | false -> search (tail @ node.Children)

        [ tree ] |> search

type Node<'T> private (id: string, value: 'T, children: ResizeArray<Node<'T>>) =

    member _.Id = id
    member _.Value = value
    member _.Children: Node<'T> seq = children :> Node<'T> seq

    static member create(id: string, value: 'T) =
        Node(id, value, ResizeArray<Node<'T>>())

    member private _.Add(child: Node<'T>) =
        if not (children |> Seq.exists (fun c -> c.Id = child.Id)) then
            children.Add child

    member this.AddChild(child: Node<'T>) =
        this.Add child
        this

    member this.AddChildren(children: Node<'T> seq) =
        children |> Seq.iter this.Add
        this

    member this.FindNode(id: string) =
        if String.IsNullOrWhiteSpace id then
            None
        else
            let parts = id.Split('.', StringSplitOptions.RemoveEmptyEntries)

            if parts.Length = 0 || parts.[0] <> this.Id then
                None
            else
                this.FindRecursive(this, parts, 1)

    member this.FindValue(id: string) =
        this.FindNode id |> Option.map (fun (v: Node<'T>) -> v.Value)

    member this.Contains(path: string) = this.FindValue(path).IsSome
    
    member private this.FindRecursive(current: Node<'T>, ids: string[], index: int) =
        if index >= ids.Length then
            Some current
        else
            current.Children
            |> Seq.tryFind (fun c -> c.Id = ids.[index])
            |> Option.bind (fun child -> this.FindRecursive(child, ids, index + 1))

module NodeBuilder =

    let withChild (child: Node<'T>) (parent: Node<'T>) = parent.AddChild child

    let withChildren (children: Node<'T> seq) (parent: Node<'T>) = parent.AddChildren children

    let findNode (nodeId: string) =
        fun (node: Node<'T>) -> node.FindNode nodeId

    let findValue (nodeId: string) =
        fun (node: Node<'T>) -> node.FindValue nodeId
