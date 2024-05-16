module Infrastructure.Configuration

open Microsoft.Extensions.Configuration

let getJsonConfiguration file =
    ConfigurationBuilder()
        .AddJsonFile(file, optional = false, reloadOnChange = true)
        .Build()

let getSection<'a> (configuration: IConfigurationRoot) sectionName =
    let section = configuration.GetSection(sectionName)

    section
    |> Option.ofObj
    |> Option.bind (fun section -> if section.Exists() then Some <| section.Get<'a>() else None)
