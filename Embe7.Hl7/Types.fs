namespace Embe7.Hl7

module Types =
    let augment len def list =
        let current = list |> List.length

        if current >= len then
            list
        else
            list @ List.replicate (len - current) def

    let replace ix v xs =
        xs |> List.mapi (fun i c -> if i = ix then v else c)

    type SubComponent =
        { Value: string }

        static member Create value = { Value = value }
        static member Create() = { Value = "" }
        static member Contents{ Value = v } = v

    type Component =
        { SubComponents: list<SubComponent> }

        static member Create subComponents = { SubComponents = subComponents }
        static member Create subComponent = { SubComponents = [ subComponent ] }

        static member Create(value: string) =
            value |> SubComponent.Create |> Component.Create

        static member Create() =
            SubComponent.Create() |> Component.Create

        static member Contents{ SubComponents = v } = v

    type FieldRepeat =
        { Components: list<Component> }

        static member Create components = { Components = components }
        static member Create comp = { Components = [ comp ] }

        static member Create(value: string) =
            value |> Component.Create |> FieldRepeat.Create

        static member Create() =
            Component.Create() |> FieldRepeat.Create

        static member Contents{ Components = v } = v

        member this.WithComponent(ix: int, value: Component) =
            augment ix (Component.Create()) this.Components
            |> replace (ix - 1) value
            |> FieldRepeat.Create

        member this.WithComponent(ix: int, value: string) =
            this.WithComponent(ix, Component.Create value)

    type Field =
        { FieldRepeats: list<FieldRepeat> }

        static member Create fieldRepeats = { FieldRepeats = fieldRepeats }
        static member Create fieldRepeat = { FieldRepeats = [ fieldRepeat ] }

        static member Create(value: string) =
            value |> FieldRepeat.Create |> Field.Create

        static member Create() = FieldRepeat.Create() |> Field.Create

        static member Contents{ FieldRepeats = v } = v

        member this.WithComponent(ix: int, value: Component) =
            match this.FieldRepeats with
            | [ x ] -> x.WithComponent(ix, value)
            | _ -> failwith "cannot add a component to a field with repetitions"
            |> Field.Create

        member this.WithComponent(ix: int, value: string) =
            this.WithComponent(ix, Component.Create value)

    type Separators =
        { Field: char
          Component: char
          SubComponent: char
          FieldRepeat: char
          Escape: char }

    let mkSeparators field comp subComponent fieldRepeat escape =
        let items = Set.ofList [ field; comp; subComponent; fieldRepeat; escape ]

        if Seq.length items <> 5 then
            None
        else
            Some(
                { Field = field
                  Component = comp
                  SubComponent = subComponent
                  FieldRepeat = fieldRepeat
                  Escape = escape }
            )

    let defaultSeparators =
        { Field = '|'
          Component = '^'
          SubComponent = '&'
          FieldRepeat = '~'
          Escape = '\\' }

    type MessageHeader =
        { Separators: Separators
          Fields: list<Field> }

        static member Create(separators, fields) =
            { Separators = separators
              Fields = fields }

        static member Create() =
            { Separators = defaultSeparators
              Fields = [] }

        static member Contents{ Separators = _; Fields = v } = v


    type MessageSegment =
        { Name: string
          Fields: list<Field> }

        static member Create(name, fields) = { Name = name; Fields = fields }
        static member Contents{ Name = _; Fields = v } = v

    type Message =
        { Header: MessageHeader
          Segments: list<MessageSegment> }

        static member Create(header, segments) =
            { Header = header; Segments = segments }

        static member Create() =
            { Header = MessageHeader.Create()
              Segments = [] }
