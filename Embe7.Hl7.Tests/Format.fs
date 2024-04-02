module FormatTests

open Xunit
open Swensen.Unquote
open Embe7.Hl7.Format
open Embe7.Hl7.Types

[<Fact>]
let ``escapeNonPrinting handles newlines`` () =
    test <@ escapeNonPrinting '\\' "\n" = "\\X0A\\" @>

[<Fact>]
let ``escapeNonPrinting escapes multiple characters`` () =
    test <@ escapeNonPrinting '\\' "\n\n" = "\\X0A0A\\" @>

[<Fact>]
let ``escapeNonPrinting handles mix`` () =
    test <@ escapeNonPrinting '\\' "note1\nnote2" = "note1\\X0A\\note2" @>

[<Fact>]
let ``formatSubComponent handles empty string`` () =
    let sc = SubComponent.Create ""
    test <@ formatSubComponent defaultSeparators sc = "" @>

[<Fact>]
let ``formatSubComponent handles simple string`` () =
    let sc = SubComponent.Create "a value"
    test <@ formatSubComponent defaultSeparators sc = "a value" @>

[<Fact>]
let ``formatSubComponent handles non printing characters`` () =
    let sc = SubComponent.Create "formatted:\n\tnote"
    test <@ formatSubComponent defaultSeparators sc = "formatted:\\X0A09\\note" @>

[<Fact>]
let ``formatSubComponent handles separators`` () =
    let sc = SubComponent.Create "^&\\~|"
    test <@ formatSubComponent defaultSeparators sc = "\\S\\\\T\\\\E\\\\R\\\\F\\" @>

[<Theory>]
[<InlineData("\\H\\", "\\H\\")>]
[<InlineData("\\Z123\\", "\\Z123\\")>]
[<InlineData("\\\\Z123\\", "\\E\\\\Z123\\")>]
[<InlineData("\\\\Z123\\\\", "\\E\\\\Z123\\\\E\\")>]
[<InlineData("\\H\\\\H\\", "\\H\\\\H\\")>]
[<InlineData("\\Z123\\\\Z456\\", "\\Z123\\\\Z456\\")>]
let ``formatSubComponent handles literal escape sequences`` str expected =
    let sc = SubComponent.Create str
    test <@ formatSubComponent defaultSeparators sc = expected @>

[<Fact>]
let ``formatSubComponent does not falsely correct things that look like literal escape sequences`` () =
    let sc = SubComponent.Create "\\X,\\"
    test <@ formatSubComponent defaultSeparators sc = "\\E\\X,\\E\\" @>

[<Fact>]
let ``formatSubComponent handles weird control characters`` () =
    let sc = SubComponent.Create "\x1f"
    test <@ formatSubComponent defaultSeparators sc = "\\X1F\\" @>

[<Fact>]
let ``formatComponent handles single empty subcomponent`` () =
    let c =
        Component.Create [ SubComponent.Create "" ]

    test <@ formatComponent defaultSeparators c = "" @>

[<Fact>]
let ``formatComponent handles single simple subcomponent`` () =
    let c =
        Component.Create [ SubComponent.Create "yep" ]

    test <@ formatComponent defaultSeparators c = "yep" @>

[<Fact>]
let ``formatComponent handles multiple subcomponents`` () =
    let c =
        Component.Create [ SubComponent.Create "yep"
                           SubComponent.Create ""
                           SubComponent.Create "here" ]

    test <@ formatComponent defaultSeparators c = "yep&&here" @>

[<Fact>]
let ``formatField handles no repetitions`` () =
    let f = Field.Create "test"

    test <@ formatField defaultSeparators f = "test" @>

[<Fact>]
let ``formatField handles repetitions`` () =
    let f =
        Field.Create [ FieldRepeat.Create "test"
                       FieldRepeat.Create "next" ]

    test <@ formatField defaultSeparators f = "test~next" @>

[<Fact>]
let ``formatField handles repetitions with different components`` () =
    let f =
        Field.Create [ FieldRepeat.Create [ Component.Create [ SubComponent.Create "a"
                                                               SubComponent.Create "d" ]
                                            Component.Create [ SubComponent.Create "b" ] ]
                       FieldRepeat.Create [ Component.Create [ SubComponent.Create "c" ] ] ]

    test <@ formatField defaultSeparators f = "a&d^b~c" @>

[<Fact>]
let ``formatMessageHeader works with no fields`` () =
    let h =
        MessageHeader.Create(defaultSeparators, [])

    test <@ formatMessageHeader h = "MSH|^~\\&|" @>

[<Fact>]
let ``formatMessageHeader works with non standard separators`` () =
    let seps =
        { Field = '*'
          FieldRepeat = '%'
          Escape = '`'
          Component = '$'
          SubComponent = '@' }

    let h =
        MessageHeader.Create(seps, [ Field.Create "test" ])

    test <@ formatMessageHeader h = "MSH*$%`@*test" @>
