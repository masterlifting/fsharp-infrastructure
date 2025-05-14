[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Exception

let toMessage (ex: exn) =
    ex.InnerException
    |> Option.ofObj
    |> Option.map _.Message
    |> Option.defaultValue ex.Message
