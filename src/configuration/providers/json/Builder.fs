module internal Infrastructure.Configuration.Providers.Builder.Json

open Microsoft.Extensions.Configuration

let addFile file (builder: IConfigurationBuilder) =
    builder.AddJsonFile(file, optional = false, reloadOnChange = true)
