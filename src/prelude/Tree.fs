[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Tree

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
    member _.Children : Node<'T> seq = children :> Node<'T> seq

    static member Create(id: string, value: 'T) =
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

type Root<'T> =
    {
        Root: Node<'T>
        Delimiter: char
    }

    static member Init(root: Node<'T>) =
        {
            Root = root
            Delimiter = '.'
        }

    member this.FindNode(id: string) =
        if System.String.IsNullOrWhiteSpace id then
            None
        else
            let ids = id.Split(this.Delimiter, System.StringSplitOptions.RemoveEmptyEntries)

            if ids.Length = 0 || ids.[0] <> this.Root.Id then
                None
            else
                this.FindRecursive(this.Root, ids, 1)

    member this.FindValue(id: string) =
       this.FindNode id
       |> Option.map (fun (v: Node<'T>) -> v.Value)

    member this.Contains(path: string) =
        this.FindValue(path).IsSome

    member private this.FindRecursive(node: Node<'T>, parts: string[], index: int) =
        if index >= parts.Length then
            Some node
        else
            let child = node.Children |> Seq.tryFind (fun c -> c.Id = parts.[index])
            match child with
            | Some c -> this.FindRecursive(c, parts, index + 1)
            | None -> None

module NodeBuilder =
    
    let withChild (child: Node<'T>) (parent: Node<'T>) = parent.AddChild child

    let withChildren (children: Node<'T> seq) (parent: Node<'T>) = parent.AddChildren children
