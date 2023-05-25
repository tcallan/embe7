module CleanTests

open Xunit
open Swensen.Unquote.Assertions
open Embe7.Hl7.Clean

[<Theory>]
[<InlineData("MSH|^~\\&|\rZZZ|\\S\\")>]
[<InlineData("MSH|^~\\&|\rZZZ|\\S\\\\S\\")>]
[<InlineData("MSH|^~\\&|\rZZZ|\\S\\foobar\\S\\")>]
[<InlineData("MSH|^~\\&|\rZZZ|\\E\\\\Zfoobar\\")>]
let ``fixImproperEscapes ignores valid escapes`` src = test <@ fixImproperEscapes src = src @>

[<Theory>]
[<InlineData("MSH|^~\\&|\rZZZ|\\s", "MSH|^~\\&|\rZZZ|\\E\\s")>]
[<InlineData("MSH|^~\\&|\rZZZ|Unit\\s|foobar\\s", "MSH|^~\\&|\rZZZ|Unit\\E\\s|foobar\\E\\s")>]
let ``fixImproperEscapes fixes trivially wrong escapes`` src fix =
    test <@ fixImproperEscapes src = fix @>

[<Fact>]
let ``fixImproperEscapes fixes improper cross field escapes`` () =
    let src = "MSH|^~\\&|\rZZZ|\\Z|nextfield\\"
    test <@ fixImproperEscapes src = "MSH|^~\\&|\rZZZ|\\E\\Z|nextfield\\E\\" @>

[<Fact>]
let ``fixImproperEscapes fixes improper cross repeat escapes`` () =
    let src = "MSH|^~\\&|\rZZZ|\\Z~nextrepeat\\"
    test <@ fixImproperEscapes src = "MSH|^~\\&|\rZZZ|\\E\\Z~nextrepeat\\E\\" @>

[<Fact>]
let ``fixImproperEscapes fixes improper cross component escapes`` () =
    let src = "MSH|^~\\&|\rZZZ|\\Z^nextcomponent\\"
    test <@ fixImproperEscapes src = "MSH|^~\\&|\rZZZ|\\E\\Z^nextcomponent\\E\\" @>

[<Fact>]
let ``fixImproperEscapes fixes improper cross subcomponent escapes`` () =
    let src = "MSH|^~\\&|\rZZZ|\\Z&nextsubcomponent\\"
    test <@ fixImproperEscapes src = "MSH|^~\\&|\rZZZ|\\E\\Z&nextsubcomponent\\E\\" @>

[<Fact>]
let ``fixImproperEscapes fixes a realistic message`` () =
    let msg =
        """MSH|^~\&|INITECH||||202111011540||SIU^S14|20211101154026797208|P|2.4||||||8859/1 
SCH||3201313||||MO^Modified|72194|Normal|30|M|^^^202111080930^202111081000|^Smith^Jane^||||^Smith^Jane^||||^Smith^Jane^||ACC-J||3331313000|Booked 
NTE|1||CT leg \\auth pending- cmw 
PID|1||A215356||Rosenbaum^Michael^^PhD^Dr||19770728|M|Michael|2106-3|1234 Road Dr^^Jacksonville^FL^12345^||(904)555-5555^^CP~^NET^X.400^drmrosenbaum@example.com||English|M|||||| 
RGS|1||
AIS|1||72194|202111080930|||30|M||Booked||
AIL|1||4^PET/CT-Mandarin|||202111080930|||30|M||Booked
AIP|1||1063466811^Bland^Robert^|^Astronaut||202111080930|||30|M||Booked"""

    let expected =
        """MSH|^~\&|INITECH||||202111011540||SIU^S14|20211101154026797208|P|2.4||||||8859/1 
SCH||3201313||||MO^Modified|72194|Normal|30|M|^^^202111080930^202111081000|^Smith^Jane^||||^Smith^Jane^||||^Smith^Jane^||ACC-J||3331313000|Booked 
NTE|1||CT leg \E\\E\auth pending- cmw 
PID|1||A215356||Rosenbaum^Michael^^PhD^Dr||19770728|M|Michael|2106-3|1234 Road Dr^^Jacksonville^FL^12345^||(904)555-5555^^CP~^NET^X.400^drmrosenbaum@example.com||English|M|||||| 
RGS|1||
AIS|1||72194|202111080930|||30|M||Booked||
AIL|1||4^PET/CT-Mandarin|||202111080930|||30|M||Booked
AIP|1||1063466811^Bland^Robert^|^Astronaut||202111080930|||30|M||Booked"""

    test <@ fixImproperEscapes msg = expected @>
