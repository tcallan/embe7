namespace Embe7.Hl7

open System.Text.RegularExpressions

// TODO: tests
module Format =
    let internal formatSeparators seps =
        $"%c{seps.FieldChar}%c{seps.ComponentChar}%c{seps.FieldRepeatChar}%c{seps.EscapeChar}%c{seps.SubComponentChar}"

    /// <summary>
    /// Replace a character that is not otherwise allowed to appear in HL7 messages with a valid
    /// escape sequence. Example: <code>escape '\\' '~' 'R' "some~value"</code> would produce
    /// <code>"some\\R\\value"</code>.
    /// </summary>
    /// <param name="escape">the HL7 escape character to use; usually '\'</param>
    /// <param name="x">the character to replace</param>
    /// <param name="escaped">the escape sequence</param>
    /// <param name="value">the string to replace values in</param>
    let escape (escape: char) (x: char) (escaped: char) (value: string) =
        let from = x |> string
        let new' = $"%c{escape}%c{escaped}%c{escape}"
        value.Replace(from, new')

    let private stringToHex s =
        s
        |> Seq.map (byte >> sprintf "%02X")
        |> String.concat ""

    let internal escapeNonPrinting (escape: char) (value: string) =
        Regex.Replace(value, "\\p{Cc}+", (fun c -> $"%c{escape}X%s{stringToHex c.Value}%c{escape}"))

    let private fixPreEscaped (escape: char) (value: string) =
        let e = escape |> string |> Regex.Escape

        let regex =
            $"%s{e}E%s{e}([TSRFH]|[XZ][\\da-fA-F]*?)%s{e}E%s{e}"

        let r (m: Match) =
            m.Groups
            |> Seq.tryItem 1
            |> Option.map (fun c -> $"%c{escape}%s{c.Value}%c{escape}")
            |> Option.defaultValue m.Value

        Regex.Replace(value, regex, r)

    let internal formatSubComponent separators { Value = value } =
        value
        |> escape separators.EscapeChar separators.EscapeChar 'E'
        |> escape separators.EscapeChar separators.SubComponentChar 'T'
        |> escape separators.EscapeChar separators.ComponentChar 'S'
        |> escape separators.EscapeChar separators.FieldRepeatChar 'R'
        |> escape separators.EscapeChar separators.FieldChar 'F'
        |> escapeNonPrinting separators.EscapeChar
        |> fixPreEscaped separators.EscapeChar

    let internal formatComponent separators { SubComponentsList = subComponents } =
        subComponents
        |> List.map (formatSubComponent separators)
        |> String.concat (separators.SubComponentChar |> string)

    let internal formatRepetition separators { ComponentsList = components } =
        components
        |> List.map (formatComponent separators)
        |> String.concat (separators.ComponentChar |> string)

    let internal formatField separators { FieldRepeatsList = repetitions } =
        repetitions
        |> List.map (formatRepetition separators)
        |> String.concat (separators.FieldRepeatChar |> string)

    let internal formatFields separators fields =
        fields
        |> List.map (formatField separators)
        |> String.concat (separators.FieldChar |> string)

    let internal formatMessageHeader { SeparatorsValue = seps; FieldsList = fields } =
        $"MSH%s{formatSeparators seps}%c{seps.FieldChar}%s{formatFields seps fields}"

    let internal formatSegment separators { NameValue = name; FieldsList = fields } =
        $"%s{name}%c{separators.FieldChar}%s{formatFields separators fields}"

    [<CompiledName("FormatMessage")>]
    let formatMessage (msg: Message) =
        formatMessageHeader msg.HeaderValue
        :: (List.map (formatSegment msg.HeaderValue.SeparatorsValue) msg.SegmentsList)
        |> String.concat "\r"
        |> (fun x -> x + "\r")
