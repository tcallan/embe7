namespace Embe7.Hl7

open FParsec

// TODO: tests
module Paths =
    type Path =
        { Segment: string
          SegmentRepeat: int option
          Field: int
          FieldRepeat: int option
          Component: int option
          SubComponent: int option }

    let private pseg = many1Chars (upper <|> digit)

    let private prepeat = between (pchar '(') (pchar ')') puint32 |>> int |> opt

    let private ppath = pchar '.' >>. pint32 |>> int

    let private parsePath =
        parse {
            let! seg = pseg
            let! segRepeat = prepeat
            let! field = ppath
            let! fieldRepeat = prepeat
            let! comp = opt ppath
            let! subComp = opt ppath
            do! eof

            return
                { Segment = seg
                  SegmentRepeat = segRepeat
                  Field = field
                  FieldRepeat = fieldRepeat
                  Component = comp
                  SubComponent = subComp }
        }

    let private optToResult errMsg opt =
        match opt with
        | Some x -> Result.Ok x
        | None -> Result.Error errMsg

    let private singleOrByIndex errMsg (i: int option) (xs: list<_>) =
        match xs, i with
        | [ x ], None -> Result.Ok x
        | _, Some n -> List.tryItem n xs |> optToResult errMsg
        | _ -> Result.Error errMsg

    let private toZeroIx = Option.map (fun x -> x - 1)

    let private getFieldStrict path index fields =
        fields
        |> List.tryItem index
        |> optToResult "invalid field"
        |> Result.bind (Field.Contents >> singleOrByIndex "invalid field repeat" (path.FieldRepeat))
        |> Result.bind (
            FieldRepeat.Contents
            >> singleOrByIndex "invalid component" (path.Component |> toZeroIx)
        )
        |> Result.bind (
            Component.Contents
            >> singleOrByIndex "invalid subcomponent" (path.SubComponent |> toZeroIx)
        )
        |> Result.map SubComponent.Contents

    let private getMshFieldStrict (message: Embe7.Hl7.Message) (path: Path) =
        match path.Field with
        | 1 -> message.Header.Separators.Field |> string |> Result.Ok
        | 2 -> message.Header.Separators |> Format.formatSeparators |> Result.Ok
        | n -> message.Header.Fields |> getFieldStrict path (n - 3)

    let private getSegmentFieldStrict (message: Embe7.Hl7.Message) (path: Path) =
        message.Segments
        |> List.filter (fun seg -> seg.Name = path.Segment)
        |> singleOrByIndex "invalid segment" (path.SegmentRepeat)
        |> Result.bind (MessageSegment.Contents >> getFieldStrict path (path.Field - 1))

    let getStrictValue (message: Embe7.Hl7.Message) (path: Path) =
        match path.Segment with
        | "MSH" -> getMshFieldStrict message path
        | _ -> getSegmentFieldStrict message path

    let private manyOrByIndex ix xs =
        match ix with
        | Some i -> List.tryItem i xs |> Option.map List.singleton |> Option.defaultValue []
        | None -> xs

    let private getFieldSmart path index fields =
        fields
        |> manyOrByIndex (Some index)
        |> List.collect (Field.Contents >> manyOrByIndex (path.FieldRepeat))
        |> List.collect (FieldRepeat.Contents >> manyOrByIndex (path.Component |> toZeroIx))
        |> List.collect (Component.Contents >> manyOrByIndex (path.SubComponent |> toZeroIx))
        |> List.map SubComponent.Contents

    let getMshFieldSmart (message: Embe7.Hl7.Message) (path: Path) =
        match path.Field with
        | 1 -> message.Header.Separators.Field |> string |> List.singleton
        | 2 -> message.Header.Separators |> Format.formatSeparators |> List.singleton
        | n -> message.Header.Fields |> getFieldSmart path (n - 3)

    let private getSegmentFieldSmart (message: Embe7.Hl7.Message) (path: Path) =
        message.Segments
        |> List.filter (fun seg -> seg.Name = path.Segment)
        |> manyOrByIndex (path.SegmentRepeat)
        |> List.collect ((MessageSegment.Contents >> getFieldSmart path (path.Field - 1)))

    let getSmartValue (message: Embe7.Hl7.Message) (path: Path) =
        match path.Segment with
        | "MSH" -> getMshFieldSmart message path
        | _ -> getSegmentFieldSmart message path
        
    let getPath path =
        match run parsePath path with
        | Success(result, _, _) -> Result.Ok result
        | Failure(err, _, _) -> Result.Error err
