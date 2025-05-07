module internal Infrastructure.Configuration.Providers.Builder.Yaml

open System
open System.IO
open System.Collections.Generic
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.Configuration

type private ConfigurationSource() =
    inherit FileConfigurationSource()

    override this.Build(builder) =
        match this.FileProvider with
        | null ->
            this.FileProvider <- builder.GetFileProvider()
            new ConfigurationProvider(this)
        | _ -> new ConfigurationProvider(this)

and private ConfigurationProvider(source) =
    inherit FileConfigurationProvider(source)

    let deserializer =
        YamlDotNet.Serialization.DeserializerBuilder().IgnoreUnmatchedProperties().Build()

    let toData (data: Dictionary<obj, obj>) =
        let result = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

        let buildNodeName parentName nodeName =
            match parentName with
            | "" -> nodeName
            | _ when nodeName = "<<" -> parentName
            | _ -> $"%s{parentName}:%s{nodeName}"

        let rec innerLoop nodeName (data: obj) =
            match data with
            | :? Dictionary<obj, obj> as dict ->
                dict
                |> Seq.iter (fun node ->
                    let nodeKey =
                        match node.Key.ToString() with
                        | null -> String.Empty
                        | key -> key

                    let nodeName = nodeName |> buildNodeName <| nodeKey
                    innerLoop nodeName node.Value)

            | :? List<obj> as list ->
                list
                |> Seq.iteri (fun index element ->
                    let nodeName = nodeName |> buildNodeName <| (index |> string)
                    innerLoop nodeName element)
            | _ -> result.Add(nodeName, data |> string)

        innerLoop "" data
        result

    override this.Load(stream: Stream) =
        use reader = new StreamReader(stream)
        let yaml = deserializer.Deserialize<Dictionary<obj, obj>>(reader)
        this.Data <- yaml |> toData

let private addFileWithOptions
    (builder: IConfigurationBuilder)
    (provider: IFileProvider option)
    (path: string)
    (optional: bool)
    (reloadOnChange: bool)
    =

    let provider, path =
        match provider, Path.IsPathRooted(path) with
        | None, true ->

            let directory =
                match Path.GetDirectoryName(path) with
                | null -> failwith "Path.GetDirectoryName returned null"
                | directory -> directory

            let provider = new PhysicalFileProvider(directory) :> IFileProvider |> Some
            provider, Path.GetFileName(path)
        | None, false -> None, path
        | Some provider, _ -> Some provider, path

    let fileProvider: IFileProvider | null =
        match provider with
        | Some provider -> provider
        | None -> null

    let source =
        ConfigurationSource(
            FileProvider = fileProvider,
            Path = path,
            Optional = optional,
            ReloadOnChange = reloadOnChange
        )

    builder.Add <| source

let addFile file builder =
    addFileWithOptions builder None file false false
