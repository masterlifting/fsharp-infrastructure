module Infrastructure.Configuration

open System
open Microsoft.Extensions.Configuration
open Infrastructure.Domain.Errors
open Infrastructure.Dsl.ActivePatterns

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
                | "" -> nodeName
                | _ -> $"{parentName}:{nodeName}"

            let rec innerLoop nodeName (data: Object) =
                match data with
                | :? Dictionary<Object, Object> as dict ->
                    dict
                    |> Seq.iter (fun node ->
                        let nodeName = nodeName |> buildNodeName <| node.Key.ToString()
                        innerLoop nodeName node.Value)

                | :? List<Object> as list ->
                    list
                    |> Seq.iteri (fun index element ->
                        let nodeName = nodeName |> buildNodeName <| index.ToString()
                        innerLoop nodeName element)
                | _ -> result.Add(nodeName, data.ToString())

            innerLoop "" data
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

let private getJsonConfiguration fileName =
    let file = $"{fileName}.json"

    ConfigurationBuilder()
        .AddJsonFile(file, optional = false, reloadOnChange = true)
        .Build()

open Yaml

let private getYamlConfiguration fileName =
    let file = $"{fileName}.yaml"
    let builder = ConfigurationBuilder()
    let builder = builder |> AddYamlFile <| file
    builder.Build()

///<summary>
/// Get configuration from a file
/// </summary>
/// <param name="fileType">File type</param>
/// <returns>Configuration</returns>
/// <exception cref="Exception">Configuration exception</exception>
let get fileType =
    match fileType with
    | Json file -> getJsonConfiguration file
    | Yaml file -> getYamlConfiguration file

let getSection<'a> sectionName (configuration: IConfigurationRoot) =
    configuration.GetSection(sectionName)
    |> Option.ofObj
    |> Option.bind (fun section -> if section.Exists() then Some <| section.Get<'a>() else None)

let getEnvVar key =
    try
        Ok
        <| match Environment.GetEnvironmentVariable(key) with
           | IsString value -> Some value
           | _ -> None
    with ex ->
        Error <| Configuration ex.Message

let getEnvVar' key configuration =
    match configuration |> getSection<string> key with
    | Some value -> Ok <| Some value
    | None -> getEnvVar key
