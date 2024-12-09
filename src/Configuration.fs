module Infrastructure.Configuration

open System
open System.Runtime.CompilerServices
open System.Text.RegularExpressions
open Microsoft.Extensions.Configuration
open Microsoft.FSharp.Reflection
open Infrastructure.Domain
open Infrastructure.Prelude

[<AutoOpen>]

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
                | _ when nodeName = "<<" -> parentName
                | _ -> $"%s{parentName}:%s{nodeName}"

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
                        let nodeName = nodeName |> buildNodeName <| (index |> string)
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
    let builder = builder |> addFile <| file
    builder.Build()

let getYaml = getYamlConfiguration
let getJson = getJsonConfiguration

let private typeHandlersMap =
    dict
        [ typeof<bool>, (box false, (fun (v: string) -> box (Convert.ToBoolean v)))
          typeof<int>, (box 0, (fun (v: string) -> box (Convert.ToInt32 v)))
          typeof<float>, (box 0.0, (fun (v: string) -> box (Convert.ToDouble v)))
          typeof<DateTime>, (box DateTime.MinValue, (fun (v: string) -> box (Convert.ToDateTime v)))
          typeof<TimeSpan>, (box TimeSpan.Zero, (fun (v: string) -> box (TimeSpan.Parse v)))
          typeof<Guid>, (box Guid.Empty, (fun (v: string) -> box (Guid.Parse v))) ]

let private get<'a> key (section: IConfigurationSection) =
    let configMap =
        section.AsEnumerable()
        |> Seq.map (fun x ->
            if String.IsNullOrEmpty(x.Value) then
                (x.Key, None)
            else
                (x.Key, Some x.Value))
        |> Map.ofSeq

    let inline findValue key =
        configMap |> Map.tryFind key |> Option.bind id

    let inline defaultValue (t: Type) =
        match typeHandlersMap.TryGetValue t with
        | true, (defaultValue, _) -> defaultValue
        | _ -> RuntimeHelpers.GetUninitializedObject t |> box

    let inline convertValue (value: string) (t: Type) =
        match typeHandlersMap.TryGetValue t with
        | true, (_, converter) -> converter value
        | _ -> Convert.ChangeType(value, t) |> box

    //TODO: Improve the Regex initialization by using a cache

    let rec getValue key type' =
        match type' with
        | valueType when valueType = typeof<string> -> findValue key |> Option.defaultValue String.Empty |> box
        | valueType when valueType.IsValueType ->
            findValue key
            |> Option.map (fun v -> convertValue v valueType)
            |> Option.defaultValue (defaultValue valueType)
        | valueType when valueType.IsArray ->
            let regex = Regex($"{key}:(\d+)$", RegexOptions.Compiled)

            let indexes =
                configMap.Keys
                |> Seq.choose (fun key ->
                    let regexMatch = regex.Match key

                    if regexMatch.Success then
                        Some(regexMatch.Groups[1].Value |> int)
                    else
                        None)
                |> Seq.distinct
                |> Array.ofSeq
                |> Array.sort

            let elementType = valueType.GetElementType()
            let result = Array.CreateInstance(elementType, indexes.Length)

            indexes
            |> Array.iteri (fun i index ->
                let value = elementType |> getValue $"{key}:{index}"
                result.SetValue(value, i))

            result |> box
        | valueType when
            valueType.IsGenericType
            && valueType.GetGenericTypeDefinition() = typedefof<Option<_>>
            ->
            let regex = Regex($"{key}:?\\w*$", RegexOptions.Compiled)

            configMap.Keys
            |> Seq.tryFind regex.IsMatch
            |> Option.map (fun _ ->
                let innerType = valueType.GetGenericArguments()[0]
                let value = innerType |> getValue key
                FSharpValue.MakeUnion(FSharpType.GetUnionCases(valueType)[1], [| value |]))
            |> Option.defaultValue None
            |> box
        | valueType ->
            let result = RuntimeHelpers.GetUninitializedObject(valueType)
            let properties = valueType.GetProperties()

            properties
            |> Array.iter (fun prop ->
                let value = prop.PropertyType |> getValue $"{key}:{prop.Name}"
                prop.SetValue(result, value))

            result

    typeof<'a> |> getValue key :?> 'a

let getSection<'a> sectionName (configuration: IConfigurationRoot) =
    configuration.GetSection(sectionName)
    |> Option.ofObj
    |> Option.bind (fun section ->
        match section.Exists() with
        | true -> section |> get<'a> sectionName |> Some
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
