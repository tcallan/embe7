module RoundTripTests

open FsCheck.Xunit
open Embe7.Hl7
open Embe7.Hl7.Format
open Embe7.Hl7.Parse
open FsCheck
open FsCheck.FSharp
open System

type MessageArb =
    static let arbs = ArbMap.defaults |> ArbMap.mergeWithType typeof<MessageArb>

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
            let! fields = Gen.nonEmptyListOf (ArbMap.generate<Field> arbs)
            let! seps = ArbMap.generate<Separators> arbs

            return MessageHeader.Create(seps, fields)
        }
        |> Arb.fromGen

    static member MessageSegment() =
        gen {
            let validNameChars = [ 'A' .. 'Z' ] @ [ '0' .. '9' ]
            let! name = Gen.nonEmptyListOf (Gen.elements validNameChars)
            let! fields = Gen.nonEmptyListOf (ArbMap.generate<Field> arbs)

            return MessageSegment.Create(name |> Array.ofList |> String, fields)
        }
        |> Arb.fromGen

    static member FieldRepeat() =
        Gen.nonEmptyListOf (ArbMap.generate<Component> arbs)
        |> Gen.map FieldRepeat.Create
        |> Arb.fromGen

    static member Field() =
        Gen.nonEmptyListOf (ArbMap.generate<FieldRepeat> arbs)
        |> Gen.map Field.Create
        |> Arb.fromGen

    static member Component() =
        Gen.nonEmptyListOf (ArbMap.generate<SubComponent> arbs)
        |> Gen.map Component.Create
        |> Arb.fromGen

    static member SubComponent() =
        ArbMap.generate<NonNull<string>> arbs
        |> Gen.map (fun s -> SubComponent.Create(s.Get))
        |> Arb.fromGen


[<Property(Arbitrary = [| typeof<MessageArb> |])>]
let ``formatMessage |> parseMessage is a noop`` (msg: Message) =
    let rt = msg |> formatMessage |> parse
    Ok(msg) = rt
