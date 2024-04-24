module Infrastructure.Configuration

open Microsoft.Extensions.Configuration

let getJsonConfiguration file =
    ConfigurationBuilder()
        .AddJsonFile(file, optional = false, reloadOnChange = true)
        .Build()

let getSection<'a> (configuration: IConfigurationRoot) sectionName =
    let section = configuration.GetSection(sectionName)
    if section.Exists() then section.Get<'a>() |> Some else None
