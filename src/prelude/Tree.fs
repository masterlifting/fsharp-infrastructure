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

    /// Create a node with children in a functional style
    static member CreateWithChildren(id: string, value: 'T, children: Node<'T> list) =
        let node = Node<'T>.Create(id, value)
        children |> List.iter node.Add
        node

    /// Create a node and add a single child
    static member CreateWith(id: string, value: 'T, child: Node<'T>) =
        Node<'T>.CreateWithChildren(id, value, [child])

    member this.Add(child: Node<'T>) =
        if not (this.Children |> Seq.exists (fun c -> c.Id = child.Id)) then
            this.Children.Add child

    /// Functional method to add children and return the parent node
    member this.WithChildren(children: Node<'T> list) =
        children |> List.iter this.Add
        this

    /// Functional method to add a single child and return the parent node
    member this.WithChild(child: Node<'T>) =
        this.Add child
        this

/// Builder functions for creating trees in a functional way
module NodeBuilder =
    
    /// Create a node with id and value
    let node id value = Node<'T>.Create(id, value)
    
    /// Create a node with children
    let nodeWith id value children = Node<'T>.CreateWithChildren(id, value, children)
    
    /// Pipe-friendly function to add a single child
    let withChild (child: Node<'T>) (parent: Node<'T>) = parent.WithChild(child)
    
    /// Pipe-friendly function to add multiple children
    let withChildren (children: Node<'T> list) (parent: Node<'T>) = parent.WithChildren(children)
    
    /// Custom operators for tree building
    
    /// Functional operator to add a child to a node: parent ++ child
    let inline (++) (parent: Node<'T>) (child: Node<'T>) = parent.WithChild(child)
    
    /// Functional operator to add multiple children to a node: parent +++ [child1; child2]
    let inline (+++) (parent: Node<'T>) (children: Node<'T> list) = parent.WithChildren(children)
    
    /// Tree building operator: parent |+ child (alternative to ++)
    let inline (|+) (parent: Node<'T>) (child: Node<'T>) = parent.WithChild(child)
    
    /// Tree building operator: parent |++ [children] (alternative to +++)
    let inline (|++) (parent: Node<'T>) (children: Node<'T> list) = parent.WithChildren(children)
    
    /// Reverse tree building operator: child +| parent (adds child to parent)
    let inline (+|) (child: Node<'T>) (parent: Node<'T>) = parent.WithChild(child)
    
    /// Collection building operator: [children] ++| parent (adds children to parent)
    let inline (++|) (children: Node<'T> list) (parent: Node<'T>) = parent.WithChildren(children)

type Root<'T> =
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