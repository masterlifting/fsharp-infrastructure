module Infrastructure.Logging

type private Provider = | Console

type Logger =
    { logTrace: string -> unit
      logDebug: string -> unit
      logInfo: string -> unit
      logWarning: string -> unit
      logError: string -> unit }

type private Level =
    | Error
    | Warning
    | Information
    | Debug
    | Trace

let private getLevel level =
    match level with
    | Some value ->
        match value with
        | "Error" -> Error
        | "Warning" -> Warning
        | "Debug" -> Debug
        | "Trace" -> Trace
        | _ -> Information
    | None -> Information

let private createLogger level log =
    match level with
    | Error ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = ignore
          logWarning = ignore
          logError = fun message -> log message Error }
    | Warning ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = ignore
          logWarning = fun message -> log message Warning
          logError = fun message -> log message Error }
    | Information ->
        { logTrace = ignore
          logDebug = ignore
          logInfo = fun message -> log message Information
          logWarning = fun message -> log message Warning
          logError = fun message -> log message Error }
    | Debug ->
        { logTrace = ignore
          logDebug = fun message -> log message Debug
          logInfo = fun message -> log message Information
          logWarning = fun message -> log message Warning
          logError = fun message -> log message Error }
    | Trace ->
        { logTrace = fun message -> log message Trace
          logDebug = fun message -> log message Debug
          logInfo = fun message -> log message Information
          logWarning = fun message -> log message Warning
          logError = fun message -> log message Error }

let private setLogger logLevel loggerProvider =

    let level = getLevel logLevel

    match loggerProvider with
    | Console ->
        let consoleLogProcessor =
            MailboxProcessor.Start(fun inbox ->
                let rec innerLoop () =
                    async {
                        let! getMessage = inbox.Receive()
                        let message = getMessage <| System.DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss")
                        printfn $"{message}"
                        return! innerLoop ()
                    }

                innerLoop ())

        let consoleLog message level =
            match level with
            | Error -> consoleLogProcessor.Post(fun timeStamp -> $"\u001b[31mError [{timeStamp}] {message}\u001b[0m")
            | Warning ->
                consoleLogProcessor.Post(fun timeStamp -> $"\u001b[33mWarning\u001b[0m [{timeStamp}] {message}")
            | Debug -> consoleLogProcessor.Post(fun timeStamp -> $"\u001b[36mDebug\u001b[0m [{timeStamp}] {message}")
            | Trace -> consoleLogProcessor.Post(fun timeStamp -> $"\u001b[90mTrace\u001b[0m [{timeStamp}] {message}")
            | _ -> consoleLogProcessor.Post(fun timeStamp -> $"\u001b[32mInfo\u001b[0m [{timeStamp}] {message}")

        level |> createLogger <| consoleLog

let getConsoleLogger logLevel = setLogger logLevel Console
