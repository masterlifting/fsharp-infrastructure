module internal Infrastructure.Logging.Builder

open Infrastructure.Logging.Domain

let parseLevel level =
    match level with
    | "Error" -> Critical
    | "Critical" -> Critical
    | "Warn" -> Warning
    | "Warning" -> Warning
    | "Debug" -> Debug
    | "Trace" -> Trace
    | "Success" -> Success
    | _ -> Information

let build level log =
    match level with
    | Critical -> {
        logTrace = ignore
        logDebug = ignore
        logInfo = ignore
        logWarning = ignore
        logSuccess = ignore
        logCritical = log Critical
      }
    | Success -> {
        logTrace = ignore
        logDebug = ignore
        logWarning = ignore
        logInfo = ignore
        logSuccess = log Success
        logCritical = log Critical
      }
    | Warning -> {
        logTrace = ignore
        logDebug = ignore
        logInfo = ignore
        logWarning = log Warning
        logSuccess = log Success
        logCritical = log Critical
      }
    | Information -> {
        logTrace = ignore
        logDebug = ignore
        logInfo = log Information
        logWarning = log Warning
        logSuccess = log Success
        logCritical = log Critical
      }
    | Debug -> {
        logTrace = ignore
        logDebug = log Debug
        logInfo = log Information
        logWarning = log Warning
        logSuccess = log Success
        logCritical = log Critical
      }
    | Trace -> {
        logTrace = log Trace
        logDebug = log Debug
        logInfo = log Information
        logWarning = log Warning
        logSuccess = log Success
        logCritical = log Critical
      }

let LogProcessor =
    MailboxProcessor.StartImmediate(fun inbox ->

        let rec innerLoop () =
            async {
                let! logMessage = inbox.Receive()

                match LoggerState with
                | Some logger -> logMessage logger
                | None -> ()

                do! innerLoop ()
            }

        innerLoop ())
