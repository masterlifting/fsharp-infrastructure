[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Graph

open Infrastructure.Domain

let buildNodeName nodeName parentName =
    match parentName with
    | None -> nodeName
    | Some parentName -> $"%s{parentName}%s{Graph.DELIMITER}%s{nodeName}"

let buildNodeNameOfSeq data = data |> String.concat Graph.DELIMITER

let splitNodeName (name: string) =
    Graph.DELIMITER |> name.Split |> Array.toList

module DFS =

    /// <summary>
    /// Tries to find a node by its full name in the graph using depth-first search.
    /// </summary>
    /// <param name="name">The full name of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let rec tryFindByName<'a when 'a :> Graph.INodeName> name (graph: Graph.Node<'a>) =
        match graph.FullName = name with
        | true -> Some graph
        | false -> graph.Children |> List.tryPick (tryFindByName name)

    /// <summary>
    /// Tries to find a node by its ID in the graph using depth-first search.
    /// </summary>
    /// <param name="nodeId">The ID of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let rec tryFindById<'a when 'a :> Graph.INodeName> nodeId (graph: Graph.Node<'a>) =
        match graph.FullId = nodeId with
        | true -> Some graph
        | false -> graph.Children |> List.tryPick (tryFindById nodeId)

module BFS =

    /// <summary>
    /// Tries to find a node by its full name in the graph using breadth-first search.
    /// </summary>
    /// <param name="name">The full name of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let tryFindByName<'a when 'a :> Graph.INodeName> name graph =
        let rec search (nodes: Graph.Node<'a> list) =
            match nodes with
            | [] -> None
            | node :: tail ->
                match node.FullName = name with
                | true -> Some node
                | false -> search (tail @ node.Children)

        [ graph ] |> search

    /// <summary>
    /// Tries to find a node by its ID in the graph using breadth-first search.
    /// </summary>
    /// <param name="nodeId">The ID of the node.</param>
    /// <param name="graph">The graph to search in.</param>
    /// <returns>The node if found, otherwise None.</returns>
    let tryFindById<'a when 'a :> Graph.INodeName> nodeId graph =
        let rec search (nodes: Graph.Node<'a> list) =
            match nodes with
            | [] -> None
            | node :: tail ->
                match node.FullId = nodeId with
                | true -> Some node
                | false -> search (tail @ node.Children)

        [ graph ] |> search
