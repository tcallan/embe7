namespace Embe7.Hl7

open System.Runtime.InteropServices
open FParsec
open System
open System.Globalization

module Parse =
    let private insertState s = setUserState s >>. preturn s

    let private segmentSep = newline

    let private mkSeparators (field, comp, fieldRepeat, escape, subComponent) =
        { FieldChar = field
          ComponentChar = comp
          SubComponentChar = subComponent
          FieldRepeatChar = fieldRepeat
          EscapeChar = escape }

    let private parseSeparators =
        parse {
            let! field = anyChar
            let! comp = noneOf [ field ] <?> "unique component character"
            let! fieldRepeat = noneOf [ field; comp ] <?> "unique field repetition character"
            let! escape = noneOf [ field; comp; fieldRepeat ] <?> "unique escape character"

            let! subComponent =
                noneOf [ field; comp; fieldRepeat; escape ] <?> "unique subcomponent character"

            return! mkSeparators (field, comp, fieldRepeat, escape, subComponent) |> insertState
        }

    let private toList seps =
        [| seps.FieldChar
           seps.ComponentChar
           seps.SubComponentChar
           seps.FieldRepeatChar
           seps.EscapeChar
           '\n'
           '\x0b'
           '\x1c' |]

    let private noDelimiters =
        parse {
            let! seps = getUserState
            return! noneOf (toList seps)
        }

    let private hexToString (h: string) =
        let toChar x =
            Int32.Parse(x, NumberStyles.HexNumber) |> char

        (if (h.Length % 2 <> 0) then ("0" + h) else h)
        |> Seq.chunkBySize 2
        |> Seq.map (String >> toChar)
        |> Seq.toArray
        |> String

    let private escapeSequence: Parser<string, Separators> =
        parse {
            let! seps = getUserState

            let hexed = pchar 'X' >>. manyChars hex |>> hexToString

            let custom = pchar 'Z' >>. manyChars hex |>> sprintf "\\Z%s\\"

            let pescape c r = pchar c >>% (r |> string)

            return!
                pescape 'E' seps.EscapeChar
                <|> pescape 'F' seps.FieldChar
                <|> pescape 'R' seps.FieldRepeatChar
                <|> pescape 'S' seps.ComponentChar
                <|> pescape 'T' seps.SubComponentChar
                <|> (pchar 'H' >>% "\\H\\")
                <|> hexed
                <|> custom
        }

    let private escaped: Parser<string, Separators> =
        parse {
            let! seps = getUserState
            let escape = pchar seps.EscapeChar
            return! between escape escape escapeSequence
        }

    let internal parseSubComponent =
        parse {
            let chars = (many1Chars noDelimiters) <|> escaped
            return! (manyStrings chars) |>> SubComponent.Create
        }

    let private parseDelimited f inner c =
        parse {
            let! seps = getUserState
            let sep = f seps |> pchar
            return! sepBy inner sep |>> c
        }

    let private parseComponent =
        parseDelimited (fun s -> s.SubComponentChar) parseSubComponent Component.Create

    let private parseFieldRepeat =
        parseDelimited (fun s -> s.ComponentChar) parseComponent FieldRepeat.Create

    let private parseField =
        parseDelimited (fun s -> s.FieldRepeatChar) parseFieldRepeat Field.Create

    let private parseFieldSep =
        parse {
            let! seps = getUserState
            return! pchar seps.FieldChar
        }

    let internal parseFields = sepBy parseField parseFieldSep

    let internal parseSegmentFields = (parseFieldSep >>. parseFields) <|>% []

    let internal parseHeader name : Parser<MessageHeader, Separators> =
        let seg = pstring name

        seg >>. parseSeparators .>> parseFieldSep .>>. (parseFields <|>% [])
        |>> MessageHeader.Create

    let internal parseMessageHeader: Parser<MessageHeader, Separators> =
        parseHeader "MSH" <?> "message header segment (MSH)"

    let private parseSegmentName: Parser<string, Separators> =
        notFollowedByString "MSH" >>. many1Chars (upper <|> digit)
        <?> "segment name other than MSH"

    let private parseBatchedSegmentName: Parser<string, Separators> =
        notFollowedByString "FHS"
        >>. notFollowedByString "BHS"
        >>. notFollowedByString "BTS"
        >>. notFollowedByString "FTS"
        >>. parseSegmentName
        <?> "segment name other than FHS, BHS, BTS, or FTS"

    let internal parseSegment: Parser<MessageSegment, Separators> =
        parseSegmentName .>>. parseSegmentFields |>> MessageSegment.Create

    let private parseBatchedSegment: Parser<MessageSegment, Separators> =
        parseBatchedSegmentName .>>. parseSegmentFields |>> MessageSegment.Create

    let private parseMessage' segmentParser =
        parseMessageHeader
        .>>. (segmentSep >>. sepEndBy segmentParser segmentSep <|>% [])
        |>> fun (header, segments) ->
            { HeaderValue = header
              SegmentsList = segments }

    let private parseMessage = parseMessage' parseSegment

    let private parseBatchedMessage = parseMessage' parseBatchedSegment

    let private parseSingleMessage = parseMessage .>> eof

    let private mllpFrameStart = pchar '\x0b' <?> "MLLP frame start"

    let private mllpFrameEnd = pchar '\x1c' .>> newline <?> "MLLP frame end"

    let private parseMllpMessages =
        many1 (between mllpFrameStart mllpFrameEnd parseMessage) .>> eof

    let private parseBatches =
        parse {
            let! header = parseHeader "BHS" .>> segmentSep <?> "batch header segment (BHS)"
            let! messages = many parseBatchedMessage <|>% []

            let! footer =
                pstring "BTS" .>> parseFieldSep .>>. parseFields |>> MessageSegment.Create
                <?> "batch trailer segment (BTS)"

            return
                { BatchHeaderValue = header
                  BatchTrailerValue = footer
                  MessagesList = messages }
        }

    let private parseBatchFile =
        parse {
            let! header = parseHeader "FHS" .>> segmentSep <?> "file header segment (FHS)"
            let! batches = sepEndBy parseBatches segmentSep <|>% []

            let! footer =
                pstring "FTS" .>> parseFieldSep .>>. parseFields .>> optional segmentSep .>> eof
                |>> MessageSegment.Create
                <?> "file trailer segment (FTS)"

            return
                { FileHeaderValue = header
                  FileTrailerValue = footer
                  BatchesList = batches }
        }

    let private parse' parser input =
        match runParserOnString parser Separators.Default "" input with
        | Success(result, _, _) -> Core.Ok result
        | Failure(err, _, _) -> Core.Error err

    [<CompiledName("Parse")>]
    let parse = parse' parseSingleMessage

    [<CompiledName("ParseMllp")>]
    let parseMllp = parse' parseMllpMessages

    [<CompiledName("ParseBatch")>]
    let parseBatch = parse' parseBatchFile

    [<CompiledName("ParseUnsafe")>]
    let parseUnsafe input =
        match parse input with
        | Core.Ok result -> result
        | Core.Error error -> failwith error

    [<CompiledName("ParseMllpUnsafe")>]
    let parseMllpUnsafe input =
        match parseMllp input with
        | Core.Ok result -> result
        | Core.Error error -> failwith error

    [<CompiledName("TryParse")>]
    let tryParse (input: string, [<Out>] result: Embe7.Hl7.Message byref) =
        match parse input with
        | Core.Ok value ->
            result <- value
            true
        | Core.Error _ -> false

    [<CompiledName("TryParseMllp")>]
    let tryParseMllp (input: string, [<Out>] result: Embe7.Hl7.Message list byref) =
        match parseMllp input with
        | Core.Ok value ->
            result <- value
            true
        | Core.Error _ -> false

    [<CompiledName("TryParseBatch")>]
    let tryParseBatch (input: string, [<Out>] result: MessageFile byref) =
        match parseBatch input with
        | Core.Ok value ->
            result <- value
            true
        | Core.Error _ -> false
