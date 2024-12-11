[<AutoOpen>]
module Infrastructure.Prelude.CE

type ResultBuilder() =
    member _.Bind(x, f) = Result.bind f x
    member _.Return x = Ok x
    member _.ReturnFrom x = x

type ResultAsyncBuilder() =
    member _.Bind(m, f) = ResultAsync.bindAsync f m
    member _.Return m = ResultAsync.mapAsync id m
    member _.ReturnFrom m = ResultAsync.wrap id m
