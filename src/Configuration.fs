module Infrastructure.Configuration

open System
open Microsoft.Extensions.Configuration

module private Yaml =
    open System.IO
    open Infrastructure.DSL.SerDe
    open System.Collections.Generic
    open Microsoft.Extensions.FileProviders

    type YamlConfigurationSource() =
        inherit FileConfigurationSource()

        override this.Build(builder: IConfigurationBuilder) =
            if this.FileProvider |> isNull then
                this.FileProvider <- builder.GetFileProvider()
                new YamlConfigurationProvider(this)
            else
                new YamlConfigurationProvider(this)

    and YamlConfigurationProvider(source) =
        inherit FileConfigurationProvider(source)

        let flattenDictionary (data: Dictionary<Object, Object>) =
            let result = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

            let buildNodeName parentName nodeName =
                match parentName with
                | None -> nodeName
                | Some parentName -> $"{parentName}:{nodeName}"

            let rec flatten prefix (data: Object) =
                match data with
                | :? Dictionary<Object, Object> as dict ->
                    dict
                    |> Seq.iter (fun x ->
                        let nodeName = prefix |> buildNodeName <| x.Key.ToString()
                        flatten (Some nodeName) x.Value)

                | :? List<Object> as list ->
                    list
                    |> Seq.iteri (fun i element ->
                        let nodeName = prefix |> buildNodeName <| i.ToString()
                        flatten (Some nodeName) element)
                | _ ->
                    let nodeName =
                        match prefix with
                        | Some prefix -> prefix
                        | None -> ""

                    result.Add(nodeName, data.ToString())

            flatten None data
            result

        override this.Load(stream: Stream) =
            let data =
                Yaml.Deserializer.Deserialize<Dictionary<Object, Object>>(new StreamReader(stream))

            this.Data <- flattenDictionary data

    let addYamlFileWithOptions
        (builder: IConfigurationBuilder)
        (provider: #IFileProvider)
        (path: string)
        (optional: bool)
        (reloadOnChange: bool)
        =

        let provider, path =
            if isNull provider && Path.IsPathRooted(path) then
                let directory = Path.GetDirectoryName(path)
                let provider = new PhysicalFileProvider(directory)
                provider, Path.GetFileName(path)
            else
                provider, path

        let source =
            YamlConfigurationSource(
                FileProvider = provider,
                Path = path,
                Optional = optional,
                ReloadOnChange = reloadOnChange
            )

        builder.Add <| source

    let AddYamlFile (builder: IConfigurationBuilder) (path: string) =
        addYamlFileWithOptions builder null path false false

open Yaml

let getJsonConfiguration file =
    ConfigurationBuilder()
        .AddJsonFile(file, optional = false, reloadOnChange = true)
        .Build()

let getYamlConfiguration file =
    let builder = ConfigurationBuilder()
    (AddYamlFile builder file).Build()

let getSection<'a> (configuration: IConfigurationRoot) sectionName =
    let section = configuration.GetSection(sectionName)

    section
    |> Option.ofObj
    |> Option.bind (fun section -> if section.Exists() then Some <| section.Get<'a>() else None)
