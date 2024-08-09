[<RequireQualifiedAccess>]
module Infrastructure.Reflection

open System.Collections.Concurrent
open Microsoft.FSharp.Reflection

let private _ucCache = ConcurrentDictionary<string, obj[]>()

let getUnionCases<'a> () =
    try
        let type' = typeof<'a>
        let a = FSharpValue.PreComputeUnionTagReader type'
        

        // _ucCache.GetOrAdd(
        //     type'.FullName,
        //     fun _ ->
        //         FSharpType.GetUnionCases(type')
        //         |> Array.map (fun info -> FSharpValue.MakeUnion(info, args))
        // )
        // |> Array.map unbox<'a>
        // |> Ok
    with ex ->
        let message = ex |> Exception.toMessage

        Error
        <| Operation
            { Message = message
              Code = Some(__SOURCE_FILE__ + __LINE__) }
