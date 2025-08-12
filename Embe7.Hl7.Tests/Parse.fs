module ParseTests

open FParsec
open Xunit
open Swensen.Unquote.Assertions
open Embe7.Hl7
open Embe7.Hl7.Parse
open System

let simpleParse p i =
    // NOTE: using eof to ensure the provided parser consumes all input
    match runParserOnString (p .>> eof) Separators.Default "test" i with
    | Success(result, _, _) -> Core.Ok result
    | Failure(err, _, _) -> Core.Error err

let mkSimpleField x =
    x
    |> SubComponent.Create
    |> Component.Create
    |> FieldRepeat.Create
    |> Field.Create

let mkComponentField xs =
    xs
    |> List.map (SubComponent.Create >> Component.Create)
    |> FieldRepeat.Create
    |> Field.Create

let emptyFields i = List.replicate i (mkSimpleField "")

[<Fact>]
let ``parseSubComponent handles simple string`` () =
    test <@ simpleParse parseSubComponent "test" = Core.Ok({ Value = "test" }) @>

[<Theory>]
[<InlineData("\\E\\", "\\")>]
[<InlineData("\\F\\", "|")>]
[<InlineData("\\S\\", "^")>]
[<InlineData("\\T\\", "&")>]
[<InlineData("\\R\\", "~")>]
let ``parseSubComponent handles simple escapes`` str expected =
    test <@ simpleParse parseSubComponent str = Core.Ok({ Value = expected }) @>

[<Fact>]
let ``parseSubComponent handles simple hex escape`` () =
    test <@ simpleParse parseSubComponent "\\Xe1\\" = Core.Ok({ Value = "á" }) @>

[<Fact>]
let ``parseSubComponent handles complex hex escape`` () =
    test <@ simpleParse parseSubComponent "\\Xe1e9edf3fa\\" = Core.Ok({ Value = "áéíóú" }) @>

[<Fact>]
let ``parseSubComponent handles mixed escapes and text`` () =
    test
        <@ simpleParse parseSubComponent "test\\S\\test\\Xe1\\" = Core.Ok({ Value = "test^testá" }) @>

[<Theory>]
[<InlineData("\\Z123\\", "\\Z123\\")>]
[<InlineData("\\H\\", "\\H\\")>]
let ``parseSubComponent handles unknown escapes`` str expected =
    test <@ simpleParse parseSubComponent str = Core.Ok({ Value = expected }) @>

[<Fact>]
let ``parseFields handles single empty field`` () =
    test <@ simpleParse parseFields "" = Core.Ok [ mkSimpleField "" ] @>

[<Fact>]
let ``parseFields handles single not empty field`` () =
    test <@ simpleParse parseFields "field" = Core.Ok [ mkSimpleField "field" ] @>

[<Fact>]
let ``parseFields handles multiple not empty fields`` () =
    test
        <@
            simpleParse parseFields "a|b|c" = Core.Ok
                [ mkSimpleField "a"; mkSimpleField "b"; mkSimpleField "c" ]
        @>

[<Fact>]
let ``parseFields handles escaped field delimiter`` () =
    test
        <@ simpleParse parseFields "a\\F\\b|c" = Core.Ok [ mkSimpleField "a|b"; mkSimpleField "c" ] @>

[<Fact>]
let ``parseFields does not allow arbitrary escapes`` () =
    let errMsg =
        [ "Error in test: Ln: 1 Col: 2"
          "\\a|b"
          " ^"
          "Expecting: 'E', 'F', 'H', 'R', 'S', 'T', 'X' or 'Z'"
          "" ]
        |> String.concat Environment.NewLine

    test <@ simpleParse parseFields "\\a|b" = Core.Error errMsg @>

[<Fact>]
let ``parseFields complex`` () =
    let msg = "a^b^c|d&e^f&g~h&j^k"

    let expected =
        [ Field.Create
              [ FieldRepeat.Create
                    [ Component.Create [ SubComponent.Create "a" ]
                      Component.Create [ SubComponent.Create "b" ]
                      Component.Create [ SubComponent.Create "c" ] ] ]
          Field.Create
              [ FieldRepeat.Create
                    [ Component.Create [ SubComponent.Create "d"; SubComponent.Create "e" ]
                      Component.Create [ SubComponent.Create "f"; SubComponent.Create "g" ] ]
                FieldRepeat.Create
                    [ Component.Create [ SubComponent.Create "h"; SubComponent.Create "j" ]
                      Component.Create [ SubComponent.Create "k" ] ] ] ]

    test <@ simpleParse parseFields msg = Core.Ok expected @>

[<Fact>]
let ``parseFields complex escapes`` () =
    let msg = "a^b\\S\\c|d&e^f&g\\R\\h&j^k"

    let expected =
        [ Field.Create
              [ FieldRepeat.Create
                    [ Component.Create [ SubComponent.Create "a" ]
                      Component.Create [ SubComponent.Create "b^c" ] ] ]
          Field.Create
              [ FieldRepeat.Create
                    [ Component.Create [ SubComponent.Create "d"; SubComponent.Create "e" ]
                      Component.Create
                          [ SubComponent.Create "f"
                            SubComponent.Create "g~h"
                            SubComponent.Create "j" ]
                      Component.Create [ SubComponent.Create "k" ] ] ] ]

    test <@ simpleParse parseFields msg = Core.Ok expected @>

[<Fact>]
let ``parseFields with hex escape`` () =
    let msg = "a\\X0A\\b"

    let expected = [ mkSimpleField "a\nb" ]

    test <@ simpleParse parseFields msg = Core.Ok expected @>

[<Fact>]
let ``parseFields trailing seperator`` () =
    let msg = "a|b|c|"

    let expected =
        [ mkSimpleField "a"; mkSimpleField "b"; mkSimpleField "c"; mkSimpleField "" ]

    test <@ simpleParse parseFields msg = Core.Ok expected @>

[<Fact>]
let ``parseMessageHeader handles a normal MSH segment`` () =
    let header =
        "MSH|^~\\&|INITECH^INITECH^GUID||INITRODE^INITRODE^GUID||20191206103634||DFT^P03|5870023|P|2.3|"

    let expected =
        MessageHeader.Create(
            { Field = '|'
              Component = '^'
              FieldRepeat = '~'
              Escape = '\\'
              SubComponent = '&' },
            [ mkComponentField [ "INITECH"; "INITECH"; "GUID" ]
              mkSimpleField ""
              mkComponentField [ "INITRODE"; "INITRODE"; "GUID" ]
              mkSimpleField ""
              mkSimpleField "20191206103634"
              mkSimpleField ""
              mkComponentField [ "DFT"; "P03" ]
              mkSimpleField "5870023"
              mkSimpleField "P"
              mkSimpleField "2.3"
              mkSimpleField "" ]
        )

    test <@ simpleParse parseMessageHeader header = Core.Ok expected @>

[<Fact>]
let ``parseMessageHeader handles a minimal MSH segment`` () =
    let header = "MSH|^~\\&|"

    let expected =
        MessageHeader.Create(
            { Field = '|'
              Component = '^'
              FieldRepeat = '~'
              Escape = '\\'
              SubComponent = '&' },
            [ mkSimpleField "" ]
        )

    test <@ simpleParse parseMessageHeader header = Core.Ok expected @>

[<Fact>]
let ``parse handles a minimal message`` () =
    let msg = "MSH|^~\\&|"

    let expected =
        { Header =
            MessageHeader.Create(
                { Field = '|'
                  Component = '^'
                  FieldRepeat = '~'
                  Escape = '\\'
                  SubComponent = '&' },
                [ mkSimpleField "" ]
            )
          Segments = [] }

    test <@ parse msg = Core.Ok expected @>

[<Fact>]
let ``parse handles a small message`` () =
    let msg = "MSH|^~\\&|\rOBX|\r"

    let expected =
        { Header =
            MessageHeader.Create(
                { Field = '|'
                  Component = '^'
                  FieldRepeat = '~'
                  Escape = '\\'
                  SubComponent = '&' },
                [ mkSimpleField "" ]
            )
          Segments = [ MessageSegment.Create("OBX", [ mkSimpleField "" ]) ] }

    test <@ parse msg = Core.Ok expected @>

[<Fact>]
let ``parse handles a real message`` () =
    let msg =
        """MSH|^~\&|INITECH|foobar|GenericApp|GenericFac|20210331145628||SIU^S17|20210331145628509744|P|2.4|0||AL|AL|
SCH|APPTID123||||||Treatment only^Treatment only^L|^^L|15|m|^^^20210406133000||||||||||||||
TQ1|1|||||15^min|20210406133000|||||||
PID|1|53356|53356||Doe^Jane^||19640808|F||B|123 Road Street^^Monroe^GA^55555^USA^||(770)555-5555||English|M|||111-00-0000|||N||||||||
AIP|1|||||||||||
AIS|1||Activity|||||||
AIG|1|||||||||||||
AIL|1||^^^|||||||||"""

    let sch =
        MessageSegment.Create(
            "SCH",
            [ mkSimpleField "APPTID123" ]
            @ emptyFields 5
            @ [ mkComponentField [ "Treatment only"; "Treatment only"; "L" ]
                mkComponentField [ ""; ""; "L" ]
                mkSimpleField "15"
                mkSimpleField "m"
                mkComponentField [ ""; ""; ""; "20210406133000" ] ]
            @ emptyFields 14
        )

    let tq1 =
        MessageSegment.Create(
            "TQ1",
            [ mkSimpleField "1" ]
            @ emptyFields 4
            @ [ mkComponentField [ "15"; "min" ]; mkSimpleField "20210406133000" ]
            @ emptyFields 7
        )

    let pid =
        MessageSegment.Create(
            "PID",
            [ mkSimpleField "1"
              mkSimpleField "53356"
              mkSimpleField "53356"
              mkSimpleField ""
              mkComponentField [ "Doe"; "Jane"; "" ]
              mkSimpleField ""
              mkSimpleField "19640808"
              mkSimpleField "F"
              mkSimpleField ""
              mkSimpleField "B"
              mkComponentField [ "123 Road Street"; ""; "Monroe"; "GA"; "55555"; "USA"; "" ]
              mkSimpleField ""
              mkSimpleField "(770)555-5555"
              mkSimpleField ""
              mkSimpleField "English"
              mkSimpleField "M"
              mkSimpleField ""
              mkSimpleField ""
              mkSimpleField "111-00-0000"
              mkSimpleField ""
              mkSimpleField ""
              mkSimpleField "N" ]
            @ emptyFields 8
        )

    let aip = MessageSegment.Create("AIP", [ mkSimpleField "1" ] @ emptyFields 11)

    let ais =
        MessageSegment.Create(
            "AIS",
            [ mkSimpleField "1"; mkSimpleField ""; mkSimpleField "Activity" ]
            @ emptyFields 7
        )

    let aig = MessageSegment.Create("AIG", [ mkSimpleField "1" ] @ emptyFields 13)

    let ail =
        MessageSegment.Create(
            "AIL",
            [ mkSimpleField "1"; mkSimpleField ""; mkComponentField [ ""; ""; ""; "" ] ]
            @ emptyFields 9
        )

    let expected =
        { Header =
            MessageHeader.Create(
                { Field = '|'
                  Component = '^'
                  FieldRepeat = '~'
                  Escape = '\\'
                  SubComponent = '&' },
                [ mkSimpleField "INITECH"
                  mkSimpleField "foobar"
                  mkSimpleField "GenericApp"
                  mkSimpleField "GenericFac"
                  mkSimpleField "20210331145628"
                  mkSimpleField ""
                  mkComponentField [ "SIU"; "S17" ]
                  mkSimpleField "20210331145628509744"
                  mkSimpleField "P"
                  mkSimpleField "2.4"
                  mkSimpleField "0"
                  mkSimpleField ""
                  mkSimpleField "AL"
                  mkSimpleField "AL"
                  mkSimpleField "" ]
            )
          Segments = [ sch; tq1; pid; aip; ais; aig; ail ] }

    test <@ parse msg = Core.Ok expected @>

[<Fact>]
let ``parse fails on multiple MSH segements`` () =
    let msg =
        """MSH|^~\&|INITECH|foobar|GenericApp|GenericFac|20210331145628||SIU^S17|20210331145628509744|P|2.4|0||AL|AL|
MSH|INITECH|foobar|GenericApp|GenericFac|20210331145628||SIU^S17|20210331145628509744|P|2.4|0||AL|AL|"""

    let errMsg =
        [ "Error in Ln: 2 Col: 1"
          "MSH|INITECH|foobar|GenericApp|GenericFac|20210331145628||SIU^S17|20210331145628"
          "^"
          "Expecting: end of input or segment name other than MSH"
          "" ]
        |> String.concat Environment.NewLine

    test <@ parse msg = Core.Error errMsg @>

[<Fact>]
let ``parseMllp handles single message`` () =
    let msg = "\x0bMSH|^~\\&|A|B|C\x1c\r"

    let expected =
        [ { Header =
              { Separators = Separators.Default
                Fields = [ mkSimpleField "A"; mkSimpleField "B"; mkSimpleField "C" ] }
            Segments = [] } ]

    test <@ parseMllp msg = Core.Ok expected @>

[<Fact>]
let ``parseMllp handles multiple messages`` () =
    let msg = "\x0bMSH|^~\\&|A|B|C\nZZZ|A\n\x1c\r\x0bMSH|^~\\&|A|B|C\nZZZ|A\n\x1c\r"

    let expected =
        { Header =
            { Separators = Separators.Default
              Fields = [ mkSimpleField "A"; mkSimpleField "B"; mkSimpleField "C" ] }
          Segments = [ MessageSegment.Create("ZZZ", [ mkSimpleField "A" ]) ] }

    test <@ parseMllp msg = Core.Ok [ expected; expected ] @>
