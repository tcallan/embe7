namespace Embe7.Hl7

open FParsec
open Types
open System
open System.Globalization

module Parse =
    let private insertState s = setUserState s >>. preturn s

    let private segmentSep = newline

    let private mkSeparators (field, comp, fieldRepeat, escape, subComponent) =
        { Field = field
          Component = comp
          SubComponent = subComponent
          FieldRepeat = fieldRepeat
          Escape = escape }

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
        [| seps.Field
           seps.Component
           seps.SubComponent
           seps.FieldRepeat
           seps.Escape
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
                pescape 'E' seps.Escape
                <|> pescape 'F' seps.Field
                <|> pescape 'R' seps.FieldRepeat
                <|> pescape 'S' seps.Component
                <|> pescape 'T' seps.SubComponent
                <|> (pchar 'H' >>% "\\H\\")
                <|> hexed
                <|> custom
        }

    let private escaped: Parser<string, Separators> =
        parse {
            let! seps = getUserState
            let escape = pchar (seps.Escape)
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
        parseDelimited (fun s -> s.SubComponent) parseSubComponent Component.Create

    let private parseFieldRepeat =
        parseDelimited (fun s -> s.Component) parseComponent FieldRepeat.Create

    let private parseField =
        parseDelimited (fun s -> s.FieldRepeat) parseFieldRepeat Field.Create

    let private parseFieldSep =
        parse {
            let! seps = getUserState
            return! pchar (seps.Field)
        }

    let internal parseFields = sepBy parseField parseFieldSep

    let internal parseMessageHeader: Parser<MessageHeader, Separators> =
        let seg = pstring "MSH"

        seg >>. parseSeparators .>> parseFieldSep .>>. (parseFields <|>% [])
        |>> MessageHeader.Create

    let private parseSegmentName: Parser<string, Separators> =
        notFollowedByString "MSH" <?> "segment name other than MSH"
        >>. many1Chars (upper <|> digit)
        .>> parseFieldSep

    let private parseSegment: Parser<MessageSegment, Separators> =
        parseSegmentName .>>. parseFields |>> MessageSegment.Create

    let private parseMessage =
        parseMessageHeader
        .>>. (segmentSep >>. sepEndBy parseSegment segmentSep <|>% [])
        |>> fun (header, segments) -> { Header = header; Segments = segments }

    let private parseSingleMessage = parseMessage .>> eof

    let private mllpFrameStart = pchar '\x0b' <?> "MLLP frame start"

    let private mllpFrameEnd = pchar '\x1c' .>> newline <?> "MLLP frame end"

    let private parseMllpMessages =
        many1 (between mllpFrameStart mllpFrameEnd parseMessage) .>> eof

    let private parse' parser input =
        match runParserOnString parser defaultSeparators "" input with
        | Success(result, _, _) -> Core.Ok result
        | Failure(err, _, _) -> Core.Error err

    let parse = parse' parseSingleMessage

    let parseMllp = parse' parseMllpMessages
