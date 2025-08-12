namespace Embe7.Hl7

open System.Text.RegularExpressions

// TODO: tests
module Format =
    let internal formatSeparators seps =
        $"%c{seps.Field}%c{seps.Component}%c{seps.FieldRepeat}%c{seps.Escape}%c{seps.SubComponent}"

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
        |> escape separators.Escape separators.Escape 'E'
        |> escape separators.Escape separators.SubComponent 'T'
        |> escape separators.Escape separators.Component 'S'
        |> escape separators.Escape separators.FieldRepeat 'R'
        |> escape separators.Escape separators.Field 'F'
        |> escapeNonPrinting separators.Escape
        |> fixPreEscaped separators.Escape

    let internal formatComponent separators { SubComponents = subComponents } =
        subComponents
        |> List.map (formatSubComponent separators)
        |> String.concat (separators.SubComponent |> string)

    let internal formatRepetition separators { Components = components } =
        components
        |> List.map (formatComponent separators)
        |> String.concat (separators.Component |> string)

    let internal formatField separators { FieldRepeats = repetitions } =
        repetitions
        |> List.map (formatRepetition separators)
        |> String.concat (separators.FieldRepeat |> string)

    let internal formatFields separators fields =
        fields
        |> List.map (formatField separators)
        |> String.concat (separators.Field |> string)

    let internal formatMessageHeader { Separators = seps; Fields = fields } =
        $"MSH%s{formatSeparators seps}%c{seps.Field}%s{formatFields seps fields}"

    let internal formatSegment separators { Name = name; Fields = fields } =
        $"%s{name}%c{separators.Field}%s{formatFields separators fields}"

    [<CompiledName("FormatMessage")>]
    let formatMessage (msg: Message) =
        formatMessageHeader msg.Header
        :: (List.map (formatSegment msg.Header.Separators) msg.Segments)
        |> String.concat "\r"
        |> (fun x -> x + "\r")
