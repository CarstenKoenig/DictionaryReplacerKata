module DictionaryReplacerKata.DictionaryReplacer

type Input = string
type Output = string

type Key = string
type Value = string
type Dictionary = Map<Key, Value>

type FormatPart
    = KonstanterText of string
    | Ersetzung of Key
    
type Format = FormatPart list

type Error
    = ParserError of message:string
    | KeyNotFoundError of key:Key


let istErsetzungStelle (input : Input) =
    input.StartsWith "$"
    
let ersetzungParsen (input : Input) : Result<{| part: FormatPart; rest: Input |}, Error> =
    let ersetzungEnde = input.IndexOf('$', 1)
    if ersetzungEnde < 0 then Error (ParserError "Ende einer Ersetzungsstelle nicht gefunden") else
    let key = input.Substring(1, ersetzungEnde - 1)
    Ok
        {|
          part = Ersetzung key
          rest = input.Substring(ersetzungEnde + 1)
        |}

let konstanterTextParsen (input : Input) : Result<{| part: FormatPart; rest: Input |}, Error> =
    let naestesDollar = input.IndexOf('$', 1)
    if naestesDollar < 0 then
        Ok
            {|
              part = KonstanterText input
              rest = ""
            |}
    else
        Ok
            {|
              part = KonstanterText (input.Substring(0, naestesDollar))
              rest = input.Substring(naestesDollar)
            |}

let naechstenPartParsen (input : Input) : Result<{| part: FormatPart; rest: Input |}, Error> =
    if istErsetzungStelle input
        then ersetzungParsen input
        else konstanterTextParsen input
    
let rec formatParsen (input : Input) : Result<Format, Error> =
    if input = "" then Ok [] else
    result {
        let! erfolg = naechstenPartParsen input
        let! restFormat = formatParsen erfolg.rest
        return erfolg.part :: restFormat
    }
    
let formatEvaluieren (dictionary : Dictionary) (format : Format) : Result<Output, Error> =
    let evalPart =
        function
        | KonstanterText text -> Ok text
        | Ersetzung key ->
            match Map.tryFind key dictionary with
            | Some found -> Ok found
            | None -> Error (KeyNotFoundError key)
    let rec fold acc parts =
        match parts with
        | [] -> Ok acc
        | part::rest ->
            result {
                let! output = evalPart part
                return! fold (acc + output) rest
            }
    fold "" format

let replace (dictionary : Dictionary) (input : Input) : Output =
    formatParsen input
    |> Result.bind (formatEvaluieren dictionary)
    |> function
        | Ok output -> output
        | Error (ParserError message) -> failwith message
        | Error (KeyNotFoundError key) -> failwithf "Verwendeter Key %s war nicht im Dictionary" key