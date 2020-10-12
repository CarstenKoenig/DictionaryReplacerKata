module DictionaryReplacerKata.DictionaryReplacer

type Input = string
type Output = string

type Key = string
type Value = string
type Dictionary = Map<Key, Value>

let replace (dictionary : Dictionary) (input : Input) : Output =
    let sucheStelle (text : string) =
        let erstesDollar = text.IndexOf '$'
        if erstesDollar < 0 then None else
        let naechstesDollar = text.IndexOf('$', erstesDollar + 1)
        if naechstesDollar < 0 then None else
        Some
            {|
              text = text.Substring(erstesDollar, naechstesDollar - erstesDollar + 1)
              key = text.Substring(erstesDollar + 1, naechstesDollar - erstesDollar - 1)
            |}
    let rec wiederholtesErsetzen text =
        match sucheStelle text with
        | None ->
            text
        | Some stelle ->
            let neuerText = text.Replace(stelle.text, dictionary.[stelle.key])
            wiederholtesErsetzen neuerText
    wiederholtesErsetzen input