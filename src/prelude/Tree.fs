[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Tree

open Infrastructure.Domain

let init<'a when 'a :> Tree.INode> (root: 'a) (children: 'a list option) =
    Tree.Node(root, children |> Option.defaultValue [] |> List.map (fun child -> Tree.Node(child, [])))

let addChild<'a when 'a :> Tree.INode> (child: 'a) (parent: Tree.Node<'a>) =
    match parent with
    | Tree.Node(value, children) -> Tree.Node(value, children @ [ Tree.Node(child, []) ])

let addChildren<'a when 'a :> Tree.INode> (children: 'a list) (parent: Tree.Node<'a>) =
    match parent with
    | Tree.Node(value, existingChildren) -> Tree.Node(value, existingChildren @ List.map (fun child -> Tree.Node(child, [])) children)

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


type Node<'T> =
    {
        Id: string
        Value: 'T
        Children: ResizeArray<Node<'T>>
    }

    static member Create(id: string, value: 'T) =
        {
            Id = id
            Value = value
            Children = ResizeArray<Node<'T>>()
        }

    member this.Add(child: Node<'T>) =
        if this.Children |> Seq.exists (fun c -> c.Id = child.Id) then
            false
        else
            this.Children.Add child
            true

type Tree<'T> =
    {
        Root: Node<'T>
        Delimiter: char
    }

    static member Create(root: Node<'T>, ?delimiter: char) =
        {
            Root = root
            Delimiter = defaultArg delimiter '.'
        }

    member this.Find(id: string) =
        if System.String.IsNullOrWhiteSpace id then
            None
        else
            let ids = id.Split(this.Delimiter, System.StringSplitOptions.RemoveEmptyEntries)

            if ids.Length = 0 || ids.[0] <> this.Root.Id then
                None
            else
                this.FindRecursive(this.Root, ids, 1)

    member this.Contains(path: string) =
        this.Find(path).IsSome

    member private this.FindRecursive(node: Node<'T>, parts: string[], index: int) =
        if index >= parts.Length then
            Some node.Value
        else
            let child = node.Children |> Seq.tryFind (fun c -> c.Id = parts.[index])
            match child with
            | Some c -> this.FindRecursive(c, parts, index + 1)
            | None -> None