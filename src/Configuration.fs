module Infrastructure.Configuration

open System
open Microsoft.Extensions.Configuration
open Microsoft.FSharp.Reflection
open System.Collections

module private Yaml =
    open System.IO
    open System.Collections.Generic
    open Microsoft.Extensions.FileProviders

    type ConfigurationSource() =
        inherit FileConfigurationSource()

        override this.Build(builder) =
            match this.FileProvider with
            | null ->
                this.FileProvider <- builder.GetFileProvider()
                new ConfigurationProvider(this)
            | _ -> new ConfigurationProvider(this)

    and ConfigurationProvider(source) =
        inherit FileConfigurationProvider(source)

        let deserializer = YamlDotNet.Serialization.DeserializerBuilder().Build()

        let toData (data: Dictionary<obj, obj>) =
            let result = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

            let buildNodeName parentName nodeName =
                match parentName with
                | "" -> nodeName
                | _ -> $"{parentName}:{nodeName}"

            let rec innerLoop nodeName (data: obj) =
                match data with
                | :? Dictionary<obj, obj> as dict ->
                    dict
                    |> Seq.iter (fun node ->
                        let nodeName = nodeName |> buildNodeName <| node.Key.ToString()
                        innerLoop nodeName node.Value)

                | :? List<obj> as list ->
                    list
                    |> Seq.iteri (fun index element ->
                        let nodeName = nodeName |> buildNodeName <| index.ToString()
                        innerLoop nodeName element)
                | _ -> result.Add(nodeName, data |> string)

            innerLoop "" data
            result

        override this.Load(stream: Stream) =

            let yaml = deserializer.Deserialize<Dictionary<obj, obj>>(new StreamReader(stream))

            this.Data <- yaml |> toData

    let addFileWithOptions
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
            ConfigurationSource(
                FileProvider = provider,
                Path = path,
                Optional = optional,
                ReloadOnChange = reloadOnChange
            )

        builder.Add <| source

    let addFile builder path =
        addFileWithOptions builder null path false false

let private getJsonConfiguration fileName =
    let file = $"{fileName}.json"

    ConfigurationBuilder()
        .AddJsonFile(file, optional = false, reloadOnChange = true)
        .Build()

let private getYamlConfiguration fileName =
    let file = $"{fileName}.yaml"
    let builder = ConfigurationBuilder()
    let builder = builder |> Yaml.addFile <| file
    builder.Build()

let getYaml = getYamlConfiguration
let getJson = getJsonConfiguration

let private deserialize<'a> (section: IConfigurationSection) key =
    let resultType = typeof<'a>

    let source =
        section.AsEnumerable() |> Seq.map (fun x -> x.Key, x.Value) |> Map.ofSeq

    let findValue key =
        match source |> Map.tryFind key with
        | Some value ->
            match String.IsNullOrEmpty value with
            | true -> None
            | false -> Some value
        | None -> None

    let rec get (type': Type) key =
        match type' with
        | t when t.Name = "String" -> findValue key |> Option.defaultValue null |> box
        | t when t.IsValueType ->
            findValue key
            |> Option.map (fun x -> Convert.ChangeType(x, t))
            |> Option.defaultValue (Activator.CreateInstance t)
            |> box
        | t when t.IsArray ->
            let regex = Text.RegularExpressions.Regex($"{key}:(\d+)$")
            let keys = source.Keys |> Seq.filter regex.IsMatch

            let indexes =
                keys
                |> Seq.map (fun key -> int <| regex.Match(key).Groups.[1].Value)
                |> Seq.sort
                |> Seq.toArray

            let elementType = t.GetElementType()

            let values = indexes |> Array.map (fun index -> get elementType $"{key}:{index}")

            let result = Array.CreateInstance(elementType, indexes.Length)

            values |> Array.iteri (fun i value -> result.SetValue(value, i))

            result |> box

        | t when t.IsGenericType && t.Name = "FSharpOption`1" ->
            let regex = Text.RegularExpressions.Regex($"{key}:\w+$")

            match source.Keys |> Seq.tryFind (regex.IsMatch) with
            | None -> None |> box
            | Some value ->
                let innerType = t.GetGenericArguments().[0]
                get innerType key |> Some |> box
        | t ->
            let result = t |> Activator.CreateInstance
            let properties = t.GetProperties()

            properties
            |> Array.iter (fun prop ->
                let key = $"{key}:{prop.Name}"
                let value = get prop.PropertyType key

                try
                    prop.SetValue(result, value)
                with ex ->
                    failwithf "Error setting property %s: %s" prop.Name ex.Message)

            result

    get resultType key :?> 'a

let getSection<'a> sectionName (configuration: IConfigurationRoot) =
    configuration.GetSection(sectionName)
    |> Option.ofObj
    |> Option.bind (fun section ->
        match section.Exists() with
        | true -> Some <| deserialize<'a> section sectionName
        | false -> None)

/// <summary>
/// Get environment variable from environment variables.
/// </summary>
/// <param name="key"> Environment variable key </param>
/// <returns> Monad with environment variable value as a string </returns>
let getEnvVar key =
    try
        Ok
        <| match Environment.GetEnvironmentVariable(key) with
           | AP.IsString value -> Some value
           | _ -> None
    with ex ->
        Error <| NotFound ex.Message

/// <summary>
/// Get environment variable from a configuration file first, then from environment variables.
/// </summary>
/// <param name="key"> Environment variable key </param>
/// <param name="configuration"> IConfigurationRoot </param>
/// <returns> Monad with environment variable value as a string </returns>
let getEnvVar' key configuration =
    match configuration |> getSection<string> key with
    | Some value -> Ok <| Some value
    | None -> getEnvVar key
