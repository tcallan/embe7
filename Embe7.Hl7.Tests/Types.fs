module TypesTests

open Xunit
open Swensen.Unquote.Assertions
open Embe7.Hl7
open Embe7.Hl7.Paths

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

[<Fact>]
let ``MessageSegment.WithField sets the proper field`` () =
    let segment = MessageSegment.Create("PID", []).WithField(10, "asdf")
    let message = Message.Create(MessageHeader.Create(), [ segment ])
    
    test <@ getPath "PID.10" |> Result.bind (getStrictValue message) = Ok("asdf") @>

[<Fact>]
let ``MessageHeader.WithField sets the proper field`` () =
    let header = MessageHeader.Create().WithField(10, "asdf")
    let message = Message.Create().WithHeader(header)

    test <@ getPath "MSH.10" |> Result.bind (getStrictValue message) = Ok("asdf") @>

[<Fact>]
let ``Component.WithSubComponent sets first subcomponent`` () =
    let comp = Component.Create().WithSubComponent(1, "test")
    let segment = MessageSegment.Create("PID", [Field.Create(FieldRepeat.Create(comp))])
    let message = Message.Create(MessageHeader.Create(), [segment])
    
    test <@ getPath "PID.1.1.1" |> Result.bind (getStrictValue message) = Ok("test") @>

[<Fact>]
let ``Component.WithSubComponent inserts empty subcomponents as needed`` () =
    let comp = Component.Create().WithSubComponent(3, "test")
    let segment = MessageSegment.Create("PID", [Field.Create(FieldRepeat.Create(comp))])
    let message = Message.Create(MessageHeader.Create(), [segment])
    
    test <@ getPath "PID.1.1.3" |> Result.bind (getStrictValue message) = Ok("test") @>

[<Fact>]
let ``FieldRepeat.WithComponent sets first component`` () =
    let fieldRepeat = FieldRepeat.Create().WithComponent(1, "test")
    let segment = MessageSegment.Create("PID", [Field.Create(fieldRepeat)])
    let message = Message.Create(MessageHeader.Create(), [segment])
    
    test <@ getPath "PID.1.1" |> Result.bind (getStrictValue message) = Ok("test") @>

[<Fact>]
let ``FieldRepeat.WithComponent inserts empty components as needed`` () =
    let fieldRepeat = FieldRepeat.Create().WithComponent(4, "test")
    let segment = MessageSegment.Create("PID", [Field.Create(fieldRepeat)])
    let message = Message.Create(MessageHeader.Create(), [segment])
    
    test <@ getPath "PID.1.4" |> Result.bind (getStrictValue message) = Ok("test") @>

[<Fact>]
let ``Field.WithFieldRepeat sets first field repeat`` () =
    let field = Field.Create().WithFieldRepeat(0, FieldRepeat.Create("test"))
    let segment = MessageSegment.Create("PID", [field])
    let message = Message.Create(MessageHeader.Create(), [segment])
    
    test <@ getPath "PID.1" |> Result.bind (getStrictValue message) = Ok("test") @>

[<Fact>]
let ``Field.WithFieldRepeat inserts empty field repeats as needed`` () =
    let field = Field.Create().WithFieldRepeat(2, FieldRepeat.Create("test"))
    let segment = MessageSegment.Create("PID", [field])
    let message = Message.Create(MessageHeader.Create(), [segment])
    
    test <@ getPath "PID.1(2)" |> Result.bind (getStrictValue message) = Ok("test") @>

[<Fact>]
let ``MessageHeader.WithField sets field separator (MSH.1)`` () =
    let header = MessageHeader.Create().WithField(1, "#")
    let message = Message.Create().WithHeader(header)

    test <@ getPath "MSH.1" |> Result.bind (getStrictValue message) = Ok("#") @>

[<Fact>]
let ``MessageHeader.WithField sets encoding characters (MSH.2)`` () =
    let header = MessageHeader.Create().WithField(2, "!@$%")
    let message = Message.Create().WithHeader(header)

    test <@ getPath "MSH.2" |> Result.bind (getStrictValue message) = Ok("!@$%") @>

[<Fact>]
let ``MessageHeader.WithField fails for invalid field separator length`` () =
    let header = MessageHeader.Create()
    
    let result = 
        try 
            header.WithField(1, "||") |> ignore
            false
        with 
        | ex -> ex.Message.Contains("single character")
    
    test <@ result @>

[<Fact>]
let ``MessageHeader.WithField fails for invalid encoding characters length`` () =
    let header = MessageHeader.Create()
    
    let result = 
        try 
            header.WithField(2, "!@$") |> ignore
            false
        with 
        | ex -> ex.Message.Contains("exactly 4 characters")
    
    test <@ result @>

[<Fact>]
let ``MessageHeader.WithField fails for duplicate separator characters`` () =
    let header = MessageHeader.Create()
    
    let result = 
        try 
            header.WithField(2, "||||") |> ignore
            false
        with 
        | ex -> ex.Message.Contains("all separators must be unique")
    
    test <@ result @>

[<Fact>]
let ``MessageHeader.WithField fails when MSH.2 creates duplicate with field separator`` () =
    let header = MessageHeader.Create() // Default field separator is '|'
    
    let result = 
        try 
            header.WithField(2, "|@$%") |> ignore  // First char duplicates field separator
            false
        with 
        | ex -> ex.Message.Contains("all separators must be unique")
    
    test <@ result @>

[<Fact>]
let ``MessageHeader.WithField fails when MSH.1 duplicates existing encoding character`` () =
    let header = MessageHeader.Create() // Default component separator is '^'
    
    let result = 
        try 
            header.WithField(1, "^") |> ignore  // Duplicates existing component separator
            false
        with 
        | ex -> ex.Message.Contains("all separators must be unique")
    
    test <@ result @>

[<Fact>]
let ``Separators.mkSeparators returns None with duplicate separators`` () =
    let result = Separators.mkSeparators '|' '|' '&' '~' '\\'  // Duplicate '|'!
    
    test <@ result = None @>

[<Fact>]
let ``Separators.mkSeparators returns Some with unique separators`` () =
    let result = Separators.mkSeparators '#' '!' '@' '$' '%'
    let expected = { FieldChar = '#'; ComponentChar = '!'; SubComponentChar = '@'; FieldRepeatChar = '$'; EscapeChar = '%' }
    
    test <@ result = Some expected @>
    
[<Fact>]
let ``Separators.Create fails with duplicate separators`` () =
    let result = 
        try 
            Separators.Create('|', '|', '&', '~', '\\') |> ignore  // Duplicate '|'!
            false
        with 
        | ex -> ex.Message.Contains("all separators must be unique")
    
    test <@ result @>

[<Fact>]
let ``MessageHeader.WithSeparators sets separators`` () =
    let validSeps = Separators.Create('#', '!', '@', '$', '%')
    
    let header = MessageHeader.Create().WithSeparators(validSeps)
    
    test <@ header.Separators = validSeps @>

[<Fact>]
let ``MessageHeader.WithField works for field 3`` () =
    let header = MessageHeader.Create().WithField(3, "test")
    let message = Message.Create().WithHeader(header)

    test <@ getPath "MSH.3" |> Result.bind (getStrictValue message) = Ok("test") @>
