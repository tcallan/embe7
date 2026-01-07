open System
open Argu
open System.IO
open Embe7.Hl7.Paths

let read stripMllp fixEscapes path =
    let contents = File.ReadAllText path

    let c =
        if stripMllp then
            Embe7.Hl7.Clean.stripMllp contents
        else
            contents

    let c' =
        if fixEscapes then
            Embe7.Hl7.Clean.fixImproperEscapes c
        else
            c

    (path, c')

let parse (path, message) =
    match Embe7.Hl7.Parse.parse message with
    | Ok m -> Ok(path, m)
    | Error m -> Error(path, m)

let parseBatch (path, message) =
    match Embe7.Hl7.Parse.parseBatch message with
    | Ok m -> Ok(path, m)
    | Error m -> Error(path, m)

let query q (path, message) = (path, getStrictValue message q)
let querySmart q (path, message) = (path, getSmartValue message q)

let interpretQuery (path, result) =
    let interpret =
        match result with
        | Ok v -> v
        | Error m -> $"missing requested message part: %s{m}"

    (path, interpret)

let bimap ok err r =
    match r with
    | Ok v -> ok v
    | Error v -> err v

let unifyResult r =
    bimap (fun (path, msg) -> (path, $"%A{msg}")) id r

let toSeqError result =
    match result with
    | Ok _ -> Seq.empty
    | Error x -> Seq.singleton x

let toSeq result =
    match result with
    | Ok x -> Seq.singleton x
    | Error _ -> Seq.empty

let report (file, message) =
    printfn $"in %s{file}:"
    printfn $"%s{message}"

let reportSmart (file, results) =
    printfn $"in %s{file}:"
    printfn $"%A{results}"

let isError m =
    match m with
    | Ok _ -> false
    | Error _ -> true

let doParseDirectory stripMllp fixEscapes batch d =
    Directory.EnumerateFiles d
    |> Seq.map (read stripMllp fixEscapes)
    |> Seq.map (
        if batch then
            parseBatch >> toSeqError
        else
            parse >> toSeqError
    )
    |> Seq.collect (fun id -> id)
    |> Seq.iter report

let doParseFile stripMllp fixEscapes batch f =
    f
    |> read stripMllp fixEscapes
    |> (if batch then
            parseBatch >> unifyResult
        else
            parse >> unifyResult)
    |> report

let doParseAction stripMllp fixEscapes batch f =
    if Directory.Exists f then
        printfn $"parsing all files in %s{f} and reporting errors"
        printfn ""
        doParseDirectory stripMllp fixEscapes batch f
    elif File.Exists f then
        printfn $"parsing %s{f}"
        printfn ""
        doParseFile stripMllp fixEscapes batch f
    else
        printfn $"path %s{f} does not exist"

let doNavigateDirectory stripMllp fixEscapes d p =
    Directory.EnumerateFiles d
    |> Seq.map (read stripMllp fixEscapes)
    |> Seq.map parse
    |> Seq.collect toSeq
    |> Seq.map (query p)
    |> Seq.map interpretQuery
    |> Seq.iter report

let doNavigateDirectorySmart stripMllp fixEscapes d p =
    Directory.EnumerateFiles d
    |> Seq.map (read stripMllp fixEscapes)
    |> Seq.map parse
    |> Seq.collect toSeq
    |> Seq.iter reportSmart

let doNavigateFile stripMllp fixEscapes f p =
    f
    |> read stripMllp fixEscapes
    |> parse
    |> Result.map (query p)
    |> Result.map interpretQuery
    |> bimap id id
    |> report

let doNavigateFileSmart stripMllp fixEscapes f p =
    f
    |> read stripMllp fixEscapes
    |> parse
    |> Result.map (querySmart p)
    |> bimap reportSmart report

let withPath p fn =
    match getPath p with
    | Ok v -> fn v
    | Error v -> printfn $"invalid query: %s{v}"

let doNavigateAction smart stripMllp fixEscapes f p =
    if Directory.Exists f then
        printfn $"parsing all files in %s{f} and querying for %s{p}"
        printfn "any files with parse errors will be silently skipped"
        printfn ""

        let fn =
            if smart then
                doNavigateDirectorySmart
            else
                doNavigateDirectory

        fn stripMllp fixEscapes f |> withPath p
    elif File.Exists f then
        printfn $"parsing %s{f} and querying for %s{p}"
        printfn ""

        let fn = if smart then doNavigateFileSmart else doNavigateFile

        fn stripMllp fixEscapes f |> withPath p
    else
        printfn $"path %s{f} does not exist"

type Option =
    | [<Mandatory>] Path of string
    | Query of string
    | Fix_Mllp
    | Fix_Escapes
    | Smart
    | Batch

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Path _ -> "a file containing an HL7 message or a directory of HL7 messages"
            | Query _ -> "HL7 query to evaluate against the provided file (e.g. 'PID.5.1')"
            | Fix_Mllp -> "strip out any MLLP characters before parsing"
            | Fix_Escapes -> "fix any invalid uses of the escape character before parsing"
            | Smart -> "allow querying for multiple message elements"
            | Batch -> "parse HL7 batch files rather than single message files"

[<EntryPoint>]
let main argv =
    let name = Environment.GetCommandLineArgs() |> Array.head |> Path.GetFileName

    let parser = ArgumentParser.Create<Option>(programName = name)

    try
        let results = parser.Parse argv

        let file = results.GetResult Path
        let query = results.TryGetResult Query
        let stripMllp = results.Contains Fix_Mllp
        let fixEscapes = results.Contains Fix_Escapes
        let smart = results.Contains Smart
        let batch = results.Contains Batch

        match (file, query) with
        | _, None -> doParseAction stripMllp fixEscapes batch file
        | _, Some q -> doNavigateAction smart stripMllp fixEscapes file q
    with e ->
        printfn $"%s{e.Message}"

    0 // return an integer exit code
