[<RequireQualifiedAccess>]
module Infrastructure.Domain.Tree

open System

type Node<'T> private (id: string, value: 'T, children: ResizeArray<Node<'T>>) =

    member _.Id = id
    member _.Value = value
    member _.Children: Node<'T> seq = children :> Node<'T> seq

    static member create(id: string, value: 'T) =
        Node(id, value, ResizeArray<Node<'T>>())

    member private _.Add(child: Node<'T>) =
        if not (children |> Seq.exists (fun c -> c.Id = child.Id)) then
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
            let parts = id.Split('.', StringSplitOptions.RemoveEmptyEntries)

            if parts.Length = 0 || parts.[0] <> this.Id then
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
            |> Seq.tryFind (fun c -> c.Id = ids.[index])
            |> Option.bind (fun child -> this.FindRecursive(child, ids, index + 1))
