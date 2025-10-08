[<RequireQualifiedAccess>]
module Infrastructure.Domain.Tree

open System

[<Literal>]
let private Delimiter = '.'

[<RequireQualifiedAccess>]
type NodeId =
    | NodeId of string

    member this.Value =
        match this with
        | NodeId id -> id

    static member create value =
        NodeId value

    static member split (NodeId id) =
        id.Split(Delimiter, StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
    
    static member contains (parts: string seq) (NodeId id) =
        let idParts = id.Split(Delimiter, StringSplitOptions.RemoveEmptyEntries) |> Array.toList
        let partsList = parts |> Seq.toList
        
        let rec isSubsequence (sub: string list) (lst: string list) =
            match sub, lst with
            | [], _ -> true
            | _, [] -> false
            | x::xs, y::ys when x = y -> isSubsequence xs ys
            | x::xs, _ ::ys -> isSubsequence (x::xs) ys
        
        isSubsequence partsList idParts

    static member combine (parts: string seq) =
        parts |> String.concat (Delimiter.ToString()) |> NodeId.create

type Node<'T> private (id: string, value: 'T, parent: Node<'T> option, children: ResizeArray<Node<'T>>) =
    member _.Id = NodeId.create id
    member _.Value = value
    member _.Parent = parent
    member _.Children: Node<'T> seq = children :> Node<'T> seq

    static member create(id: string, value: 'T) =
        Node(id, value, None, ResizeArray<Node<'T>>())

    member private this.Add(child: Node<'T>) =
        if not (children |> Seq.exists (fun c -> c.Id = child.Id)) then
            let child = Node(child.Id.Value, child.Value, Some this, child.Children |> ResizeArray)
            children.Add child

    member internal this.AddChild(child: Node<'T>) =
        this.Add child
        this

    member internal this.AddChildren(children: Node<'T> seq) =
        children |> Seq.iter this.Add
        this

    member internal this.FindNode(id: string) =
        if String.IsNullOrWhiteSpace id then
            None
        else
            let parts = id.Split(Delimiter, StringSplitOptions.RemoveEmptyEntries)

            if parts.Length = 0 || parts[0] <> this.Id.Value then
                None
            else
                this.FindRecursive(this, parts, 1)

    member internal this.FindValue(id: string) =
        this.FindNode id |> Option.map (fun (v: Node<'T>) -> v.Value)

    member internal this.Contains(path: string) = this.FindValue(path).IsSome
    
    member private this.FindRecursive(current: Node<'T>, ids: string[], index: int) =
        if index >= ids.Length then
            Some current
        else
            current.Children
            |> Seq.tryFind (fun c -> c.Id.Value = ids[index])
            |> Option.bind (fun child -> this.FindRecursive(child, ids, index + 1))
