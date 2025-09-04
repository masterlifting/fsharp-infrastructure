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
    let rec tryFind<'a when 'a :> Tree.INode> nodeId (tree: Tree.Node<'a>) =
        match tree.Id = nodeId with
        | true -> Some tree
        | false -> tree.Children |> List.tryPick (tryFind nodeId)

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
