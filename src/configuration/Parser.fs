module internal Infrastructure.Configuration.Parser

open System
open System.Runtime.CompilerServices
open System.Text.RegularExpressions
open Microsoft.Extensions.Configuration
open Microsoft.FSharp.Reflection
open Infrastructure.Prelude

let private TypeHandlersMap =
    dict [
        typeof<bool>, (false :> obj, fun (v: string | null) -> Convert.ChangeType(v, typeof<bool>))
        typeof<int>, (0 :> obj, fun (v: string | null) -> Convert.ChangeType(v, typeof<int>))
        typeof<float>, (0.0 :> obj, fun (v: string | null) -> Convert.ChangeType(v, typeof<float>))
        typeof<DateTime>, (DateTime.MinValue :> obj, fun (v: string | null) -> Convert.ChangeType(v, typeof<DateTime>))
        typeof<TimeSpan>, (TimeSpan.Zero :> obj, fun (v: string | null) -> TimeSpan.Parse(String.toDefault v))
        typeof<Guid>, (Guid.Empty :> obj, fun (v: string | null) -> Guid.Parse(String.toDefault v))
    ]

let private arrayRegexCache = Collections.Generic.Dictionary<string, Regex>()

let private genericsRegexCache = Collections.Generic.Dictionary<string, Regex>()

let parse<'a> key (section: IConfigurationSection) =
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
        match TypeHandlersMap.TryGetValue t with
        | true, (defaultValue, _) -> defaultValue
        | _ -> RuntimeHelpers.GetUninitializedObject t

    let inline convertValue (value: string | null) (t: Type) =
        match TypeHandlersMap.TryGetValue t with
        | true, (_, converter) -> converter value
        | _ -> Convert.ChangeType(value, t) |> box

    let inline getOrAddArrayRegex key =
        match arrayRegexCache.TryGetValue key with
        | true, regex -> regex
        | _ ->
            let regex = Regex($"{key}:(\\d+)$", RegexOptions.Compiled)
            arrayRegexCache.Add(key, regex)
            regex

    let inline getOrAddGenericRegex key =
        match genericsRegexCache.TryGetValue key with
        | true, regex -> regex
        | _ ->
            let regex = Regex($"{key}:?\\w*$", RegexOptions.Compiled)
            genericsRegexCache.Add(key, regex)
            regex

    let rec getValue key type' =
        match type' with
        | valueType when valueType = typeof<string> -> findValue key |> Option.defaultValue String.Empty |> box
        | valueType when valueType.IsValueType ->
            findValue key
            |> Option.map (fun v -> convertValue v valueType)
            |> Option.defaultValue (defaultValue valueType)
        | valueType when valueType.IsArray ->
            let regex = getOrAddArrayRegex key

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

            let elementType =
                match valueType.GetElementType() with
                | null -> failwith "Array element type is null"
                | elementType -> elementType

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
            let regex = getOrAddGenericRegex key

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
