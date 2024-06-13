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

let private createLogger level log =
    match level with
    | Error ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = ignore
          logWarning = ignore
          logSuccess = ignore
          logError = fun message -> log message Error }
    | Success ->
        { logTrace = ignore
          logDebug = ignore
          logWarning = ignore
          logInfo = ignore
          logSuccess = fun message -> log message Success
          logError = fun message -> log message Error }
    | Warning ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = ignore
          logWarning = fun message -> log message Warning
          logSuccess = fun message -> log message Success
          logError = fun message -> log message Error }
    | Information ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = fun message -> log message Information
          logWarning = fun message -> log message Warning
          logSuccess = fun message -> log message Success
          logError = fun message -> log message Error }
    | Debug ->
        { logTrace = ignore
          logDebug = fun message -> log message Debug
          logInfo = fun message -> log message Information
          logWarning = fun message -> log message Warning
          logSuccess = fun message -> log message Success
          logError = fun message -> log message Error }
    | Trace ->
        { logTrace = fun message -> log message Trace
          logDebug = fun message -> log message Debug
          logInfo = fun message -> log message Information
          logWarning = fun message -> log message Warning
          logSuccess = fun message -> log message Success
          logError = fun message -> log message Error }

let private configLogger logLevelstr provider =
    let logLevel = parseLevel logLevelstr

    match provider with
    | Console ->

        let log createMessage =
            createMessage <| System.DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")
            |> printfn

        let logToConsole message level =
            match level with
            | Error -> log <| fun timeStamp -> $"\u001b[31m[{timeStamp}] {message}\u001b[0m"
            | Warning -> log <| fun timeStamp -> $"\u001b[33m[{timeStamp}]\u001b[0m {message}"
            | Debug -> log <| fun timeStamp -> $"\u001b[35m[{timeStamp}]\u001b[0m {message}"
            | Trace -> log <| fun timeStamp -> $"\u001b[90m[{timeStamp}]\u001b[0m {message}"
            | Success -> log <| fun timeStamp -> $"\u001b[32m[{timeStamp}]{message}\u001b[0m"
            | _ -> log <| fun timeStamp -> $"\u001b[36m[{timeStamp}]\u001b[0m {message}"

        logger <- Some(logLevel |> createLogger <| logToConsole)

    | File ->

        let log createMessage =
            let message =
                createMessage <| System.DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")


            System.IO.File.AppendAllText("log.txt", message + System.Environment.NewLine)

        let logToFile message level =
            match level with
            | Error -> log <| fun timeStamp -> $"Error [{timeStamp}] {message}"
            | Warning -> log <| fun timeStamp -> $"Warning [{timeStamp}] {message}"
            | Debug -> log <| fun timeStamp -> $"Debug [{timeStamp}] {message}"
            | Trace -> log <| fun timeStamp -> $"Trace [{timeStamp}] {message}"
            | Success -> log <| fun timeStamp -> $"Success [{timeStamp}] {message}"
            | _ -> log <| fun timeStamp -> $"Info [{timeStamp}] {message}"

        logger <- Some(logLevel |> createLogger <| logToFile)

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

let useConsole config =
    Configuration.getSection<string> config "Logging:LogLevel:Default"
    |> configLogger
    <| Console

let useFile config =
    Configuration.getSection<string> config "Logging:LogLevel:Default"
    |> configLogger
    <| File

module Log =
    let trace m =
        logProcessor.Post <| fun logger' -> logger'.logTrace m

    let debug m =
        logProcessor.Post <| fun logger' -> logger'.logDebug m

    let info m =
        logProcessor.Post <| fun logger' -> logger'.logInfo m

    let warning m =
        logProcessor.Post <| fun logger' -> logger'.logWarning m

    let error m =
        logProcessor.Post <| fun logger' -> logger'.logError m

    let success m =
        logProcessor.Post <| fun logger' -> logger'.logSuccess m
