[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Tree

open Infrastructure.Domain

module Builder =

    let withChild (child: Tree.Node<'T>) (parent: Tree.Node<'T>) = parent.AddChild child

    let withChildren (children: Tree.Node<'T> seq) (parent: Tree.Node<'T>) = parent.AddChildren children

let findNode (nodeId: string) =
    fun (node: Tree.Node<'T>) -> node.FindNode nodeId

let findValue (nodeId: string) =
    fun (node: Tree.Node<'T>) -> node.FindValue nodeId
