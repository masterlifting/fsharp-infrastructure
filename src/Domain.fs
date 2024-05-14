module Infrastructure.Domain

type IGraphNodeName =
    abstract member Name: string

type IGraphNodeHandle =
    inherit IGraphNodeName
    abstract member IsParallel: bool
    abstract member Handle: (unit -> Async<Result<string, string>>) option

type Graph<'a when 'a :> IGraphNodeName> =
    | Graph of 'a * Graph<'a> list

    member this.Deconstructed =
        match this with
        | Graph(node, nodes) -> (node, nodes)

    member this.Current =
        match this with
        | Graph(node, _) -> node

    member this.Children =
        match this with
        | Graph(_, nodes) -> nodes

module Errors =
    type InfrastructureError =
        | InvalidResponse of string
        | InvalidRequest of string

    type LogicError =
        | NotSupported
        | NotImplemented

    type AppError =
        | InfrastructureError of InfrastructureError
        | LogicError of LogicError
