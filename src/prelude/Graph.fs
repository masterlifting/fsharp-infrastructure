[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Graph

open Infrastructure.Domain

module Node =
    module Id =
        let combine (nodeIds: Graph.NodeId seq) =
            nodeIds |> Seq.map _.Value |> String.concat Graph.DELIMITER |> Graph.NodeIdValue

        let split (value: Graph.NodeId) =
            Graph.DELIMITER
            |> value.Value.Split
            |> List.ofArray
            |> Seq.map Graph.NodeIdValue

    module Name =
        let combine (values: string seq) = values |> String.concat Graph.DELIMITER

        let split (value: string) =
            Graph.DELIMITER |> value.Split |> List.ofArray

/// <summary>
/// Represents Depth-first search (DFS) graph algorithms.
/// </summary>
module DFS =

    /// <summary>
    /// Tries to find a node by its ID in the graph using depth-first search.
    /// </summary>
    /// <param name="nodeId">Id of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let rec tryFind<'a when 'a :> Graph.INode> nodeId (graph: Graph.Node<'a>) =
        match graph.Id = nodeId with
        | true -> Some graph
        | false -> graph.Children |> List.tryPick (tryFind nodeId)

/// <summary>
/// Represents breadth-first search (BFS) graph algorithms.
/// </summary>
module BFS =

    /// <summary>
    /// Tries to find a node by its ID in the graph using breadth-first search.
    /// </summary>
    /// <param name="nodeId">Id of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let tryFind<'a when 'a :> Graph.INode> nodeId graph =
        let rec search (nodes: Graph.Node<'a> list) =
            match nodes with
            | [] -> None
            | node :: tail ->
                match node.Id = nodeId with
                | true -> Some node
                | false -> search (tail @ node.Children)

        [ graph ] |> search
