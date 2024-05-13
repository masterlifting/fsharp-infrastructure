module Infrastructure.Domain

type ITreeHandler =
    abstract member Name: string
    abstract member IsParallel: bool
    abstract member Handle: (unit -> Async<Result<string, string>>) option
    abstract member Nodes: ITreeHandler list

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
