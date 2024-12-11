[<RequireQualifiedAccess>]
module Infrastructure.Prelude.Async

let bind next asyncWorkflow =
    async {
        let! result = asyncWorkflow
        return! next result
    }

let map next asyncWorkflow =
    async {
        let! result = asyncWorkflow
        return next result
    }
