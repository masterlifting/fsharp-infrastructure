[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Option

let wrap f =
    function
    | Some x -> f x
    | None -> async { return None }
