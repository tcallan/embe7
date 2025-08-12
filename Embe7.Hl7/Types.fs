namespace Embe7.Hl7

module Types =
    let internal augment len def list =
        let current = list |> List.length

        if current >= len then
            list
        else
            list @ List.replicate (len - current) def

    let internal replace ix v xs =
        xs |> List.mapi (fun i c -> if i = ix then v else c)

type SubComponent =
    { Value: string }

    static member Create value = { Value = value }
    static member Create() = { Value = "" }
    static member Contents{ Value = v } = v

type Component =
    internal
        { SubComponentsList: list<SubComponent> }

    member this.SubComponents: seq<SubComponent> = this.SubComponentsList

    static member internal Create subComponents = { SubComponentsList = subComponents }

    static member Create subComponent =
        { SubComponentsList = [ subComponent ] }

    static member Create(subComponents: seq<SubComponent>) =
        { SubComponentsList = subComponents |> List.ofSeq }

    static member Create(value: string) =
        value |> SubComponent.Create |> Component.Create

    static member Create() =
        SubComponent.Create() |> Component.Create

    static member Contents{ SubComponentsList = v } = v

    member this.WithSubComponent(ix: int, value: SubComponent) =
        Types.augment ix (SubComponent.Create()) this.SubComponentsList
        |> Types.replace (ix - 1) value
        |> Component.Create

    member this.WithSubComponent(ix: int, value: string) =
        this.WithSubComponent(ix, SubComponent.Create value)

type FieldRepeat =
    internal
        { ComponentsList: list<Component> }

    member this.Components: seq<Component> = this.ComponentsList

    static member internal Create components = { ComponentsList = components }
    static member Create comp = { ComponentsList = [ comp ] }

    static member Create(components: seq<Component>) =
        { ComponentsList = components |> List.ofSeq }

    static member Create(value: string) =
        value |> Component.Create |> FieldRepeat.Create

    static member Create() =
        Component.Create() |> FieldRepeat.Create

    static member Contents{ ComponentsList = v } = v

    member this.WithComponent(ix: int, value: Component) =
        Types.augment ix (Component.Create()) this.ComponentsList
        |> Types.replace (ix - 1) value
        |> FieldRepeat.Create

    member this.WithComponent(ix: int, value: string) =
        this.WithComponent(ix, Component.Create value)

    member this.WithComponents(components: seq<Component>) =
        { this with
            ComponentsList = components |> List.ofSeq }

type Field =
    internal
        { FieldRepeatsList: list<FieldRepeat> }

    member this.FieldRepeats: FieldRepeat seq = this.FieldRepeatsList

    static member internal Create fieldRepeats = { FieldRepeatsList = fieldRepeats }
    static member Create fieldRepeat = { FieldRepeatsList = [ fieldRepeat ] }

    static member Create(fieldRepeats: seq<FieldRepeat>) =
        { FieldRepeatsList = fieldRepeats |> List.ofSeq }

    static member Create(value: string) =
        value |> FieldRepeat.Create |> Field.Create

    static member Create() = FieldRepeat.Create() |> Field.Create

    static member Contents{ FieldRepeatsList = v } = v

    member this.WithComponent(ix: int, value: Component) =
        match this.FieldRepeatsList with
        | [ x ] -> x.WithComponent(ix, value)
        | _ -> failwith "cannot add a component to a field with repetitions"
        |> Field.Create

    member this.WithComponent(ix: int, value: string) =
        this.WithComponent(ix, Component.Create value)

    member this.WithFieldRepeat(ix: int, value: FieldRepeat) =
        Types.augment (ix + 1) (FieldRepeat.Create()) this.FieldRepeatsList
        |> Types.replace ix value
        |> Field.Create

    member this.WithFieldRepeat(ix: int, components: seq<Component>) =
        this.WithFieldRepeat(ix, FieldRepeat.Create(components))

    member this.WithFieldRepeats(fieldRepeats: seq<FieldRepeat>) =
        { this with
            FieldRepeatsList = fieldRepeats |> List.ofSeq }

type Separators =
    internal
        { FieldChar: char
          ComponentChar: char
          SubComponentChar: char
          FieldRepeatChar: char
          EscapeChar: char }

    member this.Field = this.FieldChar
    member this.Component = this.ComponentChar
    member this.SubComponent = this.SubComponentChar
    member this.FieldRepeat = this.FieldRepeatChar
    member this.Escape = this.EscapeChar

    static member Create(field, comp, subComponent, fieldRepeat, escape) =
        match Separators.mkSeparators field comp subComponent fieldRepeat escape with
        | Some s -> s
        | None -> failwith "all separators must be unique"

    static member mkSeparators field comp subComponent fieldRepeat escape =
        let items = Set.ofList [ field; comp; subComponent; fieldRepeat; escape ]

        if Set.count items <> 5 then
            None
        else
            Some(
                { FieldChar = field
                  ComponentChar = comp
                  SubComponentChar = subComponent
                  FieldRepeatChar = fieldRepeat
                  EscapeChar = escape }
            )

    static member Default =
        { FieldChar = '|'
          ComponentChar = '^'
          SubComponentChar = '&'
          FieldRepeatChar = '~'
          EscapeChar = '\\' }

    member this.WithField(field) =
        Separators.Create(
            field,
            this.ComponentChar,
            this.SubComponentChar,
            this.FieldRepeatChar,
            this.EscapeChar
        )

    member this.WithComponent(comp) =
        Separators.Create(
            this.FieldChar,
            comp,
            this.SubComponentChar,
            this.FieldRepeatChar,
            this.EscapeChar
        )

    member this.WithSubComponent(subComponent) =
        Separators.Create(
            this.FieldChar,
            this.ComponentChar,
            subComponent,
            this.FieldRepeatChar,
            this.EscapeChar
        )

    member this.WithFieldRepeat(fieldRepeat) =
        Separators.Create(
            this.FieldChar,
            this.ComponentChar,
            this.SubComponentChar,
            fieldRepeat,
            this.EscapeChar
        )

    member this.WithEscape(escape) =
        Separators.Create(
            this.FieldChar,
            this.ComponentChar,
            this.SubComponentChar,
            this.FieldRepeatChar,
            escape
        )

type MessageHeader =
    internal
        { SeparatorsValue: Separators
          FieldsList: list<Field> }

    member this.Separators: Separators = this.SeparatorsValue
    member this.Fields: seq<Field> = this.FieldsList

    static member internal Create(separators, fields) =
        { SeparatorsValue = separators
          FieldsList = fields }

    static member Create(separators, fields: seq<Field>) =
        { SeparatorsValue = separators
          FieldsList = fields |> List.ofSeq }

    static member Create fields =
        { SeparatorsValue = Separators.Default
          FieldsList = fields |> List.ofSeq }

    static member Create() =
        { SeparatorsValue = Separators.Default
          FieldsList = [] }

    member this.WithField(ix: int, value: Field) =
        let fieldIndex = ix - 3 // MSH.1 and MSH.2 are separators; MSH.3 is the first item in FieldsList

        Types.augment (fieldIndex + 1) (Field.Create()) this.FieldsList
        |> Types.replace fieldIndex value
        |> fun fields -> { this with FieldsList = fields }

    member this.WithField(ix: int, value: string) =
        match ix with
        | 1 ->
            if value.Length = 1 then
                let newSeps = this.SeparatorsValue.WithField value[0]

                { this with SeparatorsValue = newSeps }
            else
                failwith "MSH.1 (field separator) must be a single character"
        | 2 ->
            if value.Length = 4 then
                let newSeps =
                    Separators.Create(
                        this.SeparatorsValue.FieldChar,
                        value[0],
                        value[3],
                        value[1],
                        value[2]
                    )

                { this with SeparatorsValue = newSeps }
            else
                failwith "MSH.2 (encoding characters) must be exactly 4 characters"
        | n -> this.WithField(n, Field.Create value)

    member this.WithSeparators(separators: Separators) =
        { this with
            SeparatorsValue = separators }


type MessageSegment =
    internal
        { NameValue: string
          FieldsList: list<Field> }

    member this.Name: string = this.NameValue
    member this.Fields: seq<Field> = this.FieldsList

    static member Create(name) = { NameValue = name; FieldsList = [] }

    static member internal Create(name, fields) =
        { NameValue = name
          FieldsList = fields }

    static member Create(name: string, fields: seq<Field>) =
        { NameValue = name
          FieldsList = fields |> List.ofSeq }

    static member Contents{ NameValue = _; FieldsList = v } = v

    member this.WithField(ix: int, value: Field) =
        Types.augment ix (Field.Create()) this.FieldsList
        |> Types.replace (ix - 1) value
        |> fun fields -> { this with FieldsList = fields }

    member this.WithField(ix: int, value: string) = this.WithField(ix, Field.Create value)

type Message =
    internal
        { HeaderValue: MessageHeader
          SegmentsList: list<MessageSegment> }

    member this.Header: MessageHeader = this.HeaderValue
    member this.Segments: seq<MessageSegment> = this.SegmentsList

    static member internal Create(header, segments) =
        { HeaderValue = header
          SegmentsList = segments }

    static member Create(header, segments: seq<MessageSegment>) =
        { HeaderValue = header
          SegmentsList = segments |> List.ofSeq }

    static member Create() =
        { HeaderValue = MessageHeader.Create()
          SegmentsList = [] }

    member this.WithHeader(header: MessageHeader) = { this with HeaderValue = header }
