module Infrastructure.Configuration

open Microsoft.Extensions.Configuration

let setJsonConfiguration file =
    ConfigurationBuilder()
        .AddJsonFile(file, optional = false, reloadOnChange = true)
        .Build()

let getSection<'a> (config: IConfigurationRoot) name =
    let section = config.GetSection(name)
    if section.Exists() then section.Get<'a>() |> Some else None
