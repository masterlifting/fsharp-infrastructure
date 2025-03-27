[<AutoOpen>]
module Infrastructure.Domain.Error

open System

type ErrorCode =
    | Line of path: string * file: string * line: string
    | Http of Net.HttpStatusCode
    | Custom of string

    member this.Value =
        match this with
        | Line(path, file, line) -> $"%s{path}\\%s{file}:%s{line}"
        | Http code ->
            match code |> Enum.GetName with
            | null -> $"%A{code}"
            | name -> name
        | Custom value -> value

type ErrorReason =
    { Message: string
      Code: ErrorCode option }

type Error' =
    | Operation of ErrorReason
    | Permission of ErrorReason
    | AlreadyExists of string
    | NotFound of string
    | NotSupported of string
    | NotImplemented of string
    | Canceled of string

    member this.Message =
        match this with
        | Operation reason -> $"Operation error -> %s{reason.Message}"
        | Permission reason -> $"Permission error -> %s{reason.Message}"
        | AlreadyExists msg -> $"Already exists -> %s{msg}"
        | NotFound msg -> $"Not found -> %s{msg}"
        | NotSupported msg -> $"Not supported -> %s{msg}"
        | NotImplemented msg -> $"Not implemented -> %s{msg}"
        | Canceled msg -> $"Cancelled -> %s{msg}"

    member this.MessageOnly =
        match this with
        | Operation reason -> reason.Message
        | Permission reason -> reason.Message
        | AlreadyExists msg -> msg
        | NotFound msg -> msg
        | NotSupported msg -> msg
        | NotImplemented msg -> msg
        | Canceled msg -> msg

    member this.extendMsg msg =
        match this with
        | Operation reason ->
            Operation
                { reason with
                    Message = $"%s{reason.Message} -> %s{msg}" }
        | Permission reason ->
            Permission
                { reason with
                    Message = $"%s{reason.Message} -> %s{msg}" }
        | AlreadyExists src -> AlreadyExists $"%s{src} -> %s{msg}"
        | NotFound src -> NotFound $"%s{src} -> %s{msg}"
        | NotSupported src -> NotSupported $"%s{src} -> %s{msg}"
        | NotImplemented src -> NotImplemented $"%s{src} -> %s{msg}"
        | Canceled src -> Canceled $"%s{src} -> %s{msg}"

    member this.replaceMsg msg =
        match this with
        | Operation reason -> Operation { reason with Message = msg }
        | Permission reason -> Permission { reason with Message = msg }
        | AlreadyExists _ -> AlreadyExists msg
        | NotFound _ -> NotFound msg
        | NotSupported _ -> NotSupported msg
        | NotImplemented _ -> NotImplemented msg
        | Canceled _ -> Canceled msg

    static member combine(errors: Error' list) =
        match errors.Length with
        | 0 -> "Errors in the error list" |> NotFound
        | 1 -> errors[0]
        | _ ->
            let errors =
                errors
                |> Seq.mapi (fun i error -> $"   %i{i + 1}) %s{error.Message}")
                |> String.concat Environment.NewLine

            Operation
                { Message = $"Multiple errors:{Environment.NewLine}%s{errors}"
                  Code = None }
