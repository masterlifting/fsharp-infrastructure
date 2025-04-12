module Infrastructure.Logging.Providers.Console.Provider

open System
open Infrastructure.Logging.Domain
open Infrastructure.Logging.Builder

let init level =
    Console.OutputEncoding <- Text.Encoding.UTF8

    let printMessage createMessage =
        createMessage <| DateTime.Now.ToString("MM-dd HH:mm:ss") |> printfn

    let log level message =
        let color, prefix =
            match level with
            | Critical -> "31", "CRT"
            | Warning -> "33", "WRN"
            | Debug -> "35", "DBG"
            | Trace -> "90", "TRC"
            | Success -> "32", "SCS"
            | _ -> "36", "INF"

        printMessage
        <| fun timeStamp -> $"\u001b[{color}m[%s{prefix} %s{timeStamp}]\u001b[0m %s{message}"

    LoggerState <- Some(build level log)
