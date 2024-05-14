module Infrastructure.Domain

type IName =
    abstract member Name: string

type IHandle =
    inherit IName
    abstract member IsParallel: bool
    abstract member Handle: (unit -> Async<Result<string, string>>) option

type Graph<'a when 'a :> IName> =
    | Graph of 'a * Graph<'a> list

    member this.deconstructed =
        match this with
        | Graph(node, nodes) -> (node, nodes)

    member this.current =
        match this with
        | Graph(node, _) -> node

    member this.nodes =
        match this with
        | Graph(_, nodes) -> nodes

type ITreeHandler =
    abstract member Name: string
    abstract member Nodes: ITreeHandler list
    abstract member IsParallel: bool
    abstract member Handle: (unit -> Async<Result<string, string>>) option

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
