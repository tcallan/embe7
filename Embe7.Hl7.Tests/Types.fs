module TypesTests

open Xunit
open Swensen.Unquote.Assertions
open Embe7.Hl7.Types

let mkComponentField xs =
    xs
    |> List.map (SubComponent.Create >> Component.Create)
    |> FieldRepeat.Create
    |> Field.Create

[<Fact>]
let ``Field.WithComponent sets first component`` () =
    let field = Field.Create().WithComponent(1, "test")

    test <@ field = mkComponentField [ "test" ] @>

[<Fact>]
let ``Field.WithComponent inserts empty components as needed`` () =
    let field = Field.Create().WithComponent(4, "test")

    test <@ field = mkComponentField [ ""; ""; ""; "test" ] @>
