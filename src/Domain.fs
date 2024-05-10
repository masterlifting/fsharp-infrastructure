module Infrastructure.Domain

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
