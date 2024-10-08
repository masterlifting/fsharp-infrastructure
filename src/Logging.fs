module Infrastructure.Logging

open Infrastructure.Configuration

type private Provider =
    | Console
    | File

type private Logger =
    { logTrace: string -> unit
      logDebug: string -> unit
      logInfo: string -> unit
      logWarning: string -> unit
      logCritical: string -> unit
      logSuccess: string -> unit }

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
    | Critical ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = ignore
          logWarning = ignore
          logSuccess = ignore
          logCritical = log Critical }
    | Success ->
        { logTrace = ignore
          logDebug = ignore
          logWarning = ignore
          logInfo = ignore
          logSuccess = log Success
          logCritical = log Critical }
    | Warning ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = ignore
          logWarning = log Warning
          logSuccess = log Success
          logCritical = log Critical }
    | Information ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = log Information
          logWarning = log Warning
          logSuccess = log Success
          logCritical = log Critical }
    | Debug ->
        { logTrace = ignore
          logDebug = log Debug
          logInfo = log Information
          logWarning = log Warning
          logSuccess = log Success
          logCritical = log Critical }
    | Trace ->
        { logTrace = log Trace
          logDebug = log Debug
          logInfo = log Information
          logWarning = log Warning
          logSuccess = log Success
          logCritical = log Critical }

let private configLogger provider logLevel =
    let level = parseLevel logLevel

    match provider with
    | Console ->

        System.Console.OutputEncoding <- System.Text.Encoding.UTF8

        let logMessage createMessage =
            createMessage <| System.DateTime.Now.ToString("MM-dd HH:mm:ss") |> printfn

        let log level message =
            match level with
            | Critical ->
                logMessage
                <| fun timeStamp -> $"\u001b[31m[CRT %s{timeStamp}] %s{message}\u001b[0m"
            | Warning ->
                logMessage
                <| fun timeStamp -> $"\u001b[33m[WRN %s{timeStamp}]\u001b[0m %s{message}"
            | Debug ->
                logMessage
                <| fun timeStamp -> $"\u001b[35m[DBG %s{timeStamp}]\u001b[0m %s{message}"
            | Trace ->
                logMessage
                <| fun timeStamp -> $"\u001b[90m[TRC %s{timeStamp}]\u001b[0m %s{message}"
            | Success ->
                logMessage
                <| fun timeStamp -> $"\u001b[32m[SCS %s{timeStamp}] %s{message}\u001b[0m"
            | _ ->
                logMessage
                <| fun timeStamp -> $"\u001b[36m[INF %s{timeStamp}]\u001b[0m %s{message}"

        logger <- Some(create level log)

    | File ->

        let logMessage createMessage =
            let message = createMessage <| System.DateTime.Now.ToString("MM-dd HH:mm:ss")

            System.IO.File.AppendAllText("log.txt", message + System.Environment.NewLine)

        let log level message =
            match level with
            | Critical -> logMessage <| fun timeStamp -> $"[CRT %s{timeStamp}] %s{message}"
            | Warning -> logMessage <| fun timeStamp -> $"[WRN %s{timeStamp}] %s{message}"
            | Debug -> logMessage <| fun timeStamp -> $"[DBG %s{timeStamp}] %s{message}"
            | Trace -> logMessage <| fun timeStamp -> $"[TRC %s{timeStamp}] %s{message}"
            | Success -> logMessage <| fun timeStamp -> $"[SCS %s{timeStamp}] %s{message}"
            | _ -> logMessage <| fun timeStamp -> $"[INF %s{timeStamp}] %s{message}"

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
        logProcessor.Post <| fun logger' -> logger'.logTrace msg

    let debug msg =
        logProcessor.Post <| fun logger' -> logger'.logDebug msg

    let info msg =
        logProcessor.Post <| fun logger' -> logger'.logInfo msg

    let warning msg =
        logProcessor.Post <| fun logger' -> logger'.logWarning msg

    let critical msg =
        logProcessor.Post <| fun logger' -> logger'.logCritical msg

    let success msg =
        logProcessor.Post <| fun logger' -> logger'.logSuccess msg
