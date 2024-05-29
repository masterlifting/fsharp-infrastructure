module Infrastructure.Configuration

open System
open Microsoft.Extensions.Configuration

type File =
    | Json of string
    | Yaml of string

module private Yaml =
    open System.IO
    open System.Collections.Generic
    open Microsoft.Extensions.FileProviders

    type YamlConfigurationSource() =
        inherit FileConfigurationSource()

        override this.Build(builder) =
            match this.FileProvider with
            | null ->
                this.FileProvider <- builder.GetFileProvider()
                new YamlConfigurationProvider(this)
            | _ -> new YamlConfigurationProvider(this)

    and YamlConfigurationProvider(source) =
        inherit FileConfigurationProvider(source)

        let deserializer = YamlDotNet.Serialization.DeserializerBuilder().Build()

        let toData (data: Dictionary<Object, Object>) =
            let result = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

            let buildNodeName parentName nodeName =
                match parentName with
                | None -> nodeName
                | Some parentName -> $"{parentName}:{nodeName}"

            let rec innerLoop nodeName (data: Object) =
                match data with
                | :? Dictionary<Object, Object> as dict ->
                    dict
                    |> Seq.iter (fun node ->
                        let nodeName = nodeName |> buildNodeName <| node.Key.ToString()
                        innerLoop (Some nodeName) node.Value)

                | :? List<Object> as list ->
                    list
                    |> Seq.iteri (fun index element ->
                        let nodeName = nodeName |> buildNodeName <| index.ToString()
                        innerLoop (Some nodeName) element)
                | _ ->
                    let nodeName =
                        match nodeName with
                        | Some prefix -> prefix
                        | None -> ""

                    result.Add(nodeName, data.ToString())

            innerLoop None data
            result

        override this.Load(stream: Stream) =

            let yaml =
                deserializer.Deserialize<Dictionary<Object, Object>>(new StreamReader(stream))

            this.Data <- yaml |> toData

    let addYamlFileWithOptions
        (builder: IConfigurationBuilder)
        (provider: IFileProvider)
        (path: string)
        (optional: bool)
        (reloadOnChange: bool)
        =

        let provider, path =
            match provider, Path.IsPathRooted(path) with
            | null, true ->
                let directory = Path.GetDirectoryName(path)
                let provider = new PhysicalFileProvider(directory) :> IFileProvider
                provider, Path.GetFileName(path)
            | _ -> provider, path

        let source =
            YamlConfigurationSource(
                FileProvider = provider,
                Path = path,
                Optional = optional,
                ReloadOnChange = reloadOnChange
            )

        builder.Add <| source

    let AddYamlFile builder path =
        addYamlFileWithOptions builder null path false false

open Yaml

let private getJsonConfiguration fileName =
    let file = $"{fileName}.json"

    ConfigurationBuilder()
        .AddJsonFile(file, optional = false, reloadOnChange = true)
        .Build()

let private getYamlConfiguration fileName =
    let file = $"{fileName}.yaml"
    let builder = ConfigurationBuilder()
    (AddYamlFile builder file).Build()


let get fileType =
    match fileType with
    | Json file -> getJsonConfiguration file
    | Yaml file -> getYamlConfiguration file

let getSection<'a> (configuration: IConfigurationRoot) sectionName =
    let section = configuration.GetSection(sectionName)

    section
    |> Option.ofObj
    |> Option.bind (fun section -> if section.Exists() then Some <| section.Get<'a>() else None)
