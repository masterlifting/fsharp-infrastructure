module Infrastructure.Logging

open System
open Infrastructure.Configuration

type private Provider =
    | Console
    | File

type private Logger = {
    logTrace: string -> unit
    logDebug: string -> unit
    logInfo: string -> unit
    logWarning: string -> unit
    logCritical: string -> unit
    logSuccess: string -> unit
}

type private Level =
    | Critical
    | Warning
    | Information
    | Debug
    | Trace
    | Success

let mutable private logger: Logger option = None

let private parseLevel level =
    match level with
    | Some value ->
        match value with
        | "Error" -> Critical
        | "Critical" -> Critical
        | "Warn" -> Warning
        | "Warning" -> Warning
        | "Debug" -> Debug
        | "Trace" -> Trace
        | "Success" -> Success
        | _ -> Information
    | None -> Information

let private create level log =
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

let private configLogger provider logLevel =
    let level = parseLevel logLevel

    match provider with
    | Console ->

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

        logger <- Some(create level log)

    | File ->

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

        logger <- Some(create level log)

let private logProcessor =
    MailboxProcessor.StartImmediate(fun inbox ->

        let rec innerLoop () =
            async {
                let! logMessage = inbox.Receive()

                match logger with
                | Some logger' -> logMessage logger'
                | None -> ()

                do! innerLoop ()
            }

        innerLoop ())

[<Literal>]
let private sectionName = "Logging:LogLevel:Default"

let useConsole configuration =
    configuration |> getSection<string> sectionName |> configLogger Console

let useFile configuration =
    configuration |> getSection<string> sectionName |> configLogger File

[<RequireQualifiedAccess>]
module Log =
    let trace msg =
        logProcessor.Post <| fun l -> l.logTrace msg

    let debug msg =
        logProcessor.Post <| fun l -> l.logDebug msg

    let info msg =
        logProcessor.Post <| fun l -> l.logInfo msg

    let warning msg =
        logProcessor.Post <| fun l -> l.logWarning msg

    let critical msg =
        logProcessor.Post <| fun l -> l.logCritical msg

    let success msg =
        logProcessor.Post <| fun l -> l.logSuccess msg
