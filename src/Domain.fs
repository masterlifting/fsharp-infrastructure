module Infrastructure.Domain

type ITree =
    abstract member Name: string

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
