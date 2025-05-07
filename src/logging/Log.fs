[<RequireQualifiedAccess>]
module Infrastructure.Logging.Log

open Infrastructure.Logging.Builder

let trc msg =
    LogProcessor.Post <| fun l -> l.logTrace msg

let dbg msg =
    LogProcessor.Post <| fun l -> l.logDebug msg

let inf msg =
    LogProcessor.Post <| fun l -> l.logInfo msg

let wrn msg =
    LogProcessor.Post <| fun l -> l.logWarning msg

let crt msg =
    LogProcessor.Post <| fun l -> l.logCritical msg

let scs msg =
    LogProcessor.Post <| fun l -> l.logSuccess msg
