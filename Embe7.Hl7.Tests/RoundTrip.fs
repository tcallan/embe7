module RoundTripTests

open FsCheck.Xunit
open Embe7.Hl7.Types
open Embe7.Hl7.Format
open Embe7.Hl7.Parse
open FsCheck
open System

type MessageArb =
    static member Separators() =
        let allChars = [ 'z'; '|'; '^'; '~'; '\\'; '&'; '/'; ';'; '*'; ','; '.' ]

        let five g =
            Gen.map5 (fun a b c d e -> (a, b, c, d, e)) g g g g g

        let distinct (a, b, c, d, e) =
            (List.distinct [ a; b; c; d; e ] |> List.length) = 5

        let gen =
            Gen.elements allChars
            |> five
            |> Gen.filter distinct
            |> Gen.map (fun (a, b, c, d, e) ->
                { Field = a
                  FieldRepeat = b
                  Component = c
                  SubComponent = d
                  Escape = e })

        Arb.fromGen gen

    static member MessageHeader() =
        gen {
            let! fields = Gen.nonEmptyListOf (Arb.generate<Field>)
            let! seps = Arb.generate<Separators>

            return MessageHeader.Create(seps, fields)
        }
        |> Arb.fromGen

    static member MessageSegment() =
        gen {
            let validNameChars = [ 'A' .. 'Z' ] @ [ '0' .. '9' ]
            let! name = Gen.nonEmptyListOf (Gen.elements validNameChars)
            let! fields = Gen.nonEmptyListOf (Arb.generate<Field>)

            return MessageSegment.Create(name |> Array.ofList |> String, fields)
        }
        |> Arb.fromGen

    static member FieldRepeat() =
        Gen.nonEmptyListOf (Arb.generate<Component>)
        |> Gen.map FieldRepeat.Create
        |> Arb.fromGen

    static member Field() =
        Gen.nonEmptyListOf (Arb.generate<FieldRepeat>)
        |> Gen.map Field.Create
        |> Arb.fromGen

    static member Component() =
        Gen.nonEmptyListOf (Arb.generate<SubComponent>)
        |> Gen.map Component.Create
        |> Arb.fromGen

    static member SubComponent() =
        Arb.generate<NonNull<string>>
        |> Gen.map (fun s -> SubComponent.Create(s.Get))
        |> Arb.fromGen


[<Property(Arbitrary = [| typeof<MessageArb> |])>]
let ``formatMessage |> parseMessage is a noop`` (msg: Message) =
    let rt = msg |> formatMessage |> parse
    Ok(msg) = rt
