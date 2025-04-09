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
    /// Tries to find a node by its full name in the graph using depth-first search.
    /// </summary>
    /// <param name="name">The full name of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let rec tryFindByName<'a when 'a :> Graph.INode> name (graph: Graph.Node<'a>) =
        match graph.Name = name with
        | true -> Some graph
        | false -> graph.Children |> List.tryPick (tryFindByName name)

    /// <summary>
    /// Tries to find a node by its ID in the graph using depth-first search.
    /// </summary>
    /// <param name="nodeId">Id of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let rec tryFindById<'a when 'a :> Graph.INode> nodeId (graph: Graph.Node<'a>) =
        match graph.Id = nodeId with
        | true -> Some graph
        | false -> graph.Children |> List.tryPick (tryFindById nodeId)

    /// <summary>
    /// Gets all vertices in the graph starting from a given node ID.
    /// </summary>
    /// <param name="nodeId">Id of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>A list of vertices starting from the given node ID.</returns>
    let rec getVertices<'a when 'a :> Graph.INode> nodeId (graph: Graph.Node<'a>) =
        match graph |> tryFindById nodeId with
        | Some node ->
            match node.Children.Length > 0 with
            | true -> node.Children |> List.collect (fun child -> child |> getVertices child.Id)
            | false -> [ node ]
        | None -> [ graph ]

/// <summary>
/// Represents breadth-first search (BFS) graph algorithms.
/// </summary>
module BFS =

    /// <summary>
    /// Tries to find a node by its full name in the graph using breadth-first search.
    /// </summary>
    /// <param name="name">The full name of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let tryFindByName<'a when 'a :> Graph.INode> name graph =
        let rec search (nodes: Graph.Node<'a> list) =
            match nodes with
            | [] -> None
            | node :: tail ->
                match node.Name = name with
                | true -> Some node
                | false -> search (tail @ node.Children)

        [ graph ] |> search

    /// <summary>
    /// Tries to find a node by its ID in the graph using breadth-first search.
    /// </summary>
    /// <param name="nodeId">Id of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let tryFindById<'a when 'a :> Graph.INode> nodeId graph =
        let rec search (nodes: Graph.Node<'a> list) =
            match nodes with
            | [] -> None
            | node :: tail ->
                match node.Id = nodeId with
                | true -> Some node
                | false -> search (tail @ node.Children)

        [ graph ] |> search

    /// <summary>
    /// Gets all vertices in the graph starting from a given node ID.
    /// </summary>
    /// <param name="nodeId">Id of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>A list of vertices starting from the given node ID.</returns>
    let rec getVertices<'a when 'a :> Graph.INode> nodeId graph =
        match graph |> tryFindById nodeId with
        | Some node ->
            match node.Children.Length > 0 with
            | true -> node.Children |> List.collect (fun child -> child |> getVertices child.Id)
            | false -> [ node ]
        | None -> [ graph ]
