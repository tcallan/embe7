namespace Embe7.Hl7

open Types
open System.Text.RegularExpressions

// TODO: tests
module Format =
    let internal formatSeparators seps =
        sprintf "%c%c%c%c%c" seps.Field seps.Component seps.FieldRepeat seps.Escape seps.SubComponent

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
        let new' = sprintf "%c%c%c" escape escaped escape
        value.Replace(from, new')

    let private stringToHex s =
        s
        |> Seq.map (byte >> sprintf "%02X")
        |> String.concat ""

    let internal escapeNonPrinting (escape: char) (value: string) =
        Regex.Replace(value, "\\p{Cc}+", (fun c -> sprintf "%cX%s%c" escape (stringToHex c.Value) escape))

    let private fixPreEscaped (escape: char) (value: string) =
        let e = escape |> string |> Regex.Escape

        let regex =
            sprintf "%sE%s([TSRFH]|[XZ].*?)%sE%s" e e e e

        let r (m: Match) =
            m.Groups
            |> Seq.tryItem 1
            |> Option.map (fun c -> sprintf "%c%s%c" escape c.Value escape)
            |> Option.defaultValue m.Value

        Regex.Replace(value, regex, r)

    let formatSubComponent separators ({ Value = value }) =
        value
        |> escape (separators.Escape) (separators.Escape) 'E'
        |> escape (separators.Escape) (separators.SubComponent) 'T'
        |> escape (separators.Escape) (separators.Component) 'S'
        |> escape (separators.Escape) (separators.FieldRepeat) 'R'
        |> escape (separators.Escape) (separators.Field) 'F'
        |> escapeNonPrinting (separators.Escape)
        |> fixPreEscaped (separators.Escape)

    let formatComponent separators ({ SubComponents = subComponents }) =
        subComponents
        |> List.map (formatSubComponent separators)
        |> String.concat (separators.SubComponent |> string)

    let formatRepetition separators ({ Components = components }) =
        components
        |> List.map (formatComponent separators)
        |> String.concat (separators.Component |> string)

    let formatField separators ({ FieldRepeats = repetitions }) =
        repetitions
        |> List.map (formatRepetition separators)
        |> String.concat (separators.FieldRepeat |> string)

    let formatFields separators fields =
        fields
        |> List.map (formatField separators)
        |> String.concat (separators.Field |> string)

    let formatMessageHeader ({ Separators = seps; Fields = fields }) =
        sprintf "MSH%s%c%s" (formatSeparators seps) (seps.Field) (formatFields seps fields)

    let formatSegment separators ({ Name = name; Fields = fields }) =
        sprintf "%s%c%s" name (separators.Field) (formatFields separators fields)

    let formatMessage (msg: Message) =
        formatMessageHeader (msg.Header)
        :: (List.map (formatSegment msg.Header.Separators) (msg.Segments))
        |> String.concat "\r"
        |> (fun x -> x + "\r")
