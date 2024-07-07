module Infrastructure.Logging

type private Provider =
    | Console
    | File

type private Logger =
    { logTrace: string -> unit
      logDebug: string -> unit
      logInfo: string -> unit
      logWarning: string -> unit
      logError: string -> unit
      logSuccess: string -> unit }

type private Level =
    | Error
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
        | "Error" -> Error
        | "Warning" -> Warning
        | "Debug" -> Debug
        | "Trace" -> Trace
        | "Success" -> Success
        | _ -> Information
    | None -> Information

let private create level log =
    match level with
    | Error ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = ignore
          logWarning = ignore
          logSuccess = ignore
          logError = log Error }
    | Success ->
        { logTrace = ignore
          logDebug = ignore
          logWarning = ignore
          logInfo = ignore
          logSuccess = log Success
          logError = log Error }
    | Warning ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = ignore
          logWarning = log Warning
          logSuccess = log Success
          logError = log Error }
    | Information ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = log Information
          logWarning = log Warning
          logSuccess = log Success
          logError = log Error }
    | Debug ->
        { logTrace = ignore
          logDebug = log Debug
          logInfo = log Information
          logWarning = log Warning
          logSuccess = log Success
          logError = log Error }
    | Trace ->
        { logTrace = log Trace
          logDebug = log Debug
          logInfo = log Information
          logWarning = log Warning
          logSuccess = log Success
          logError = log Error }

let private configLogger provider logLevel =
    let level = parseLevel logLevel

    match provider with
    | Console ->

        System.Console.OutputEncoding <- System.Text.Encoding.UTF8
        
        let logMessage createMessage =
            createMessage <| System.DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss") |> printfn

        let log level message =
            match level with
            | Error -> logMessage <| fun timeStamp -> $"\u001b[31m[%s{timeStamp}] %s{message}\u001b[0m"
            | Warning -> logMessage <| fun timeStamp -> $"\u001b[33m[%s{timeStamp}]\u001b[0m %s{message}"
            | Debug -> logMessage <| fun timeStamp -> $"\u001b[35m[%s{timeStamp}]\u001b[0m %s{message}"
            | Trace -> logMessage <| fun timeStamp -> $"\u001b[90m[%s{timeStamp}]\u001b[0m %s{message}"
            | Success -> logMessage <| fun timeStamp -> $"\u001b[32m[%s{timeStamp}] %s{message}\u001b[0m"
            | _ -> logMessage <| fun timeStamp -> $"\u001b[36m[%s{timeStamp}]\u001b[0m %s{message}"

        logger <- Some(create level log)

    | File ->

        let logMessage createMessage =
            let message = createMessage <| System.DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")

            System.IO.File.AppendAllText("log.txt", message + System.Environment.NewLine)

        let log level message =
            match level with
            | Error -> logMessage <| fun timeStamp -> $"Error [%s{timeStamp}] %s{message}"
            | Warning -> logMessage <| fun timeStamp -> $"Warning [%s{timeStamp}] %s{message}"
            | Debug -> logMessage <| fun timeStamp -> $"Debug [%s{timeStamp}] %s{message}"
            | Trace -> logMessage <| fun timeStamp -> $"Trace [%s{timeStamp}] %s{message}"
            | Success -> logMessage <| fun timeStamp -> $"Success [%s{timeStamp}] %s{message}"
            | _ -> logMessage <| fun timeStamp -> $"Info [%s{timeStamp}] %s{message}"

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

open Infrastructure.Configuration

[<Literal>]
let sectionName = "Logging:LogLevel:Default"

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

    let error msg =
        logProcessor.Post <| fun logger' -> logger'.logError msg

    let success msg =
        logProcessor.Post <| fun logger' -> logger'.logSuccess msg
