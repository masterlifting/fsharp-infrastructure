module Infrastructure.Logging.Domain

type Level =
    | Critical
    | Warning
    | Information
    | Debug
    | Trace
    | Success

type internal Logger = {
    logTrace: string -> unit
    logDebug: string -> unit
    logInfo: string -> unit
    logWarning: string -> unit
    logCritical: string -> unit
    logSuccess: string -> unit
}

[<Literal>]
let internal CFG_DEFAULT_SECTION_NAME = "Logging:LogLevel:Default"

let mutable internal LoggerState: Logger option = None
