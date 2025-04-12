module Infrastructure.Logging.Providers.File.Provider

open System
open Infrastructure.Logging.Domain
open Infrastructure.Logging.Builder

let init level =
    let printMessage createMessage =
        let message = createMessage <| DateTime.Now.ToString("MM-dd HH:mm:ss")
        IO.File.AppendAllText("log.txt", message + Environment.NewLine)

    let log level message =
        let prefix =
            match level with
            | Critical -> "CRT"
            | Warning -> "WRN"
            | Debug -> "DBG"
            | Trace -> "TRC"
            | Success -> "SCS"
            | _ -> "INF"

        printMessage <| fun timeStamp -> $"[%s{prefix} %s{timeStamp}] %s{message}"

    LoggerState <- Some(build level log)
