[<RequireQualifiedAccess>]
module Infrastructure.Domain.Tree

open System

[<Literal>]
let private Delimiter = '.'

[<RequireQualifiedAccess>]
type NodeId =
    | NodeId of string

    /// <summary>
    /// Gets the full string value of the NodeId.
    /// </summary>
    member this.Value =
        match this with
        | NodeId id -> id

    /// <summary>
    /// Gets the individual string values of the NodeId.
    /// </summary>
    member this.Values =
        match this with
        | NodeId id -> id.Split(Delimiter, StringSplitOptions.RemoveEmptyEntries) |> Array.toList

    /// <summary>
    /// Gets the current value of the NodeId (the last segment).
    /// </summary>
    member this.CurrentValue =
        match this with
        | NodeId id ->
            id.Split(Delimiter, StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryLast
            |> Option.defaultValue this.Value

    static member create value = NodeId value

    static member split(NodeId id) =
        id.Split(Delimiter, StringSplitOptions.RemoveEmptyEntries) |> Array.toList

    static member contains (parts: string seq) (NodeId id) =
        let idParts =
            id.Split(Delimiter, StringSplitOptions.RemoveEmptyEntries) |> Array.toList
        let partsList = parts |> Seq.toList

        let rec isSubsequence (sub: string list) (lst: string list) =
            match sub, lst with
            | [], _ -> true
            | _, [] -> false
            | x :: xs, y :: ys when x = y -> isSubsequence xs ys
            | x :: xs, _ :: ys -> isSubsequence (x :: xs) ys

        isSubsequence partsList idParts

    static member combine(parts: string seq) =
        parts |> String.concat (Delimiter.ToString()) |> NodeId.create

    override this.ToString() = this.Value

type Node<'T> private (id: string, value: 'T, parent: Node<'T> option, children: ResizeArray<Node<'T>>) =
    member _.Id: NodeId =
        match parent with
        | None -> id
        | Some p -> $"{p.Id}{Delimiter}{id}"
        |> NodeId.create
    member _.Value = value
    member _.Parent = parent
    member this.Children: Node<'T> seq =
        children
        |> Seq.map (fun c -> Node(c.Id.CurrentValue, c.Value, Some this, c.Children |> ResizeArray))
    static member Empty = Node("", Unchecked.defaultof<'T>, None, ResizeArray<Node<'T>>())

    static member create(id: string, value: 'T) =
        Node(id, value, None, ResizeArray<Node<'T>>())

    member private this.Add(child: Node<'T>) =
        if not (children |> Seq.exists (fun c -> c.Id = child.Id)) then
            let child =
                Node(child.Id.CurrentValue, child.Value, Some this, child.Children |> ResizeArray)
            children.Add child

    member internal this.AddChild(child: Node<'T>) =
        this.Add child
        this

    member internal this.AddChildren(children: Node<'T> seq) =
        children |> Seq.iter this.Add
        this

    member internal this.FindNode(id: string) =
        let parts = id.Split(Delimiter, StringSplitOptions.RemoveEmptyEntries)
        this.FindRecursive(this, parts, 0)

    member internal this.FindValue(id: string) =
        this.FindNode id |> Option.map (fun (v: Node<'T>) -> v.Value)

    member internal this.Contains(path: string) = this.FindValue(path).IsSome

    member private this.FindRecursive(node: Node<'T>, ids: string[], index: int) =
        match index with
        | i when i >= ids.Length -> None
        | i when ids[i] <> node.Id.CurrentValue -> None
        | i when i = ids.Length - 1 -> Some node
        | i ->
            node.Children
            |> Seq.tryPick (fun child ->
                if child.Id.CurrentValue = ids[i + 1] then
                    this.FindRecursive(child, ids, i + 1)
                else
                    None)
