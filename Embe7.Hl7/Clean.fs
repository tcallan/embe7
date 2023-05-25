namespace Embe7.Hl7

open System.Text.RegularExpressions

module Clean =
    /// Remove any MLLP framing characters from a message
    let stripMllp (msg: string) =
        msg.Replace("\x0b", "").Replace("\x1c", "")

    let (|Prefix|_|) (prefix: string) (s: string) =
        if s.StartsWith(prefix) then
            Some(s.Substring(prefix.Length))
        else
            None

    let fixImproperEscapes msg =
        let regex = @"(?<!\\[EFSTRHXZ][^\\|~&\^]*)(\\)(?![EFSTRHXZ][^\\|~&\^]*\\)"

        let prefix = "MSH|^~\\&|"

        match msg with
        | Prefix prefix rest -> prefix + Regex.Replace(rest, regex, "\\E\\")
        | _ -> msg
