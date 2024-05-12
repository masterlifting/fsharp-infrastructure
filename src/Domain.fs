module Infrastructure.Domain

type IParallelOrSequential =
    abstract member Name: string option
    abstract member IsParallel: bool

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
