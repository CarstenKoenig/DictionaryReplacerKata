module DictionaryReplacerKata.DictionaryReplacer

open System.Text.RegularExpressions

type Input = string
type Output = string

type Key = string
type Value = string
type Dictionary = Map<Key, Value>

let replace (dictionary : Dictionary) (input : Input) : Output =
    let evaluator (m : Match) =
        dictionary.[m.Groups.["key"].Value]
    Regex.Replace(input, "\$(?<key>[^\$]+)\$", evaluator)