let lookback    = 2
let textLenght  = 10000 // words
let delim       = [|' '; '\n'; '\t';'\r'|]

let split d (s: string) = s.Split(d)

let readFileWords delim = System.Console.In.ReadToEnd >> split delim >> Seq.filter (fun l -> l <> "")
//let readFileWords delim () = @"C:\Projects\rprose\kjbible.txt" |> System.IO.File.ReadAllText |> split delim |> Seq.filter (fun l -> l <> "")

open System.Collections.Generic
open System.Text

type Keys = string list
type Tokens = List<string>
type PlaceCache = Dictionary<Keys, Tokens>

let addToken t (l:Tokens) = l.Add(t); l

let addToDict (dict:PlaceCache) (k, v) =
    let found, values = dict.TryGetValue(k)
    if found then addToken v values |> ignore
             else dict.Add (k, addToken v (Tokens()))
    dict

// Build a map using the N - 1 items in (a1,... aN) as key and aN as value
let splitLastItem a = let n = Array.length a
                      Array.sub a 0 (n - 1) |> Array.toList, Array.get a (n - 1)

let createDict () = readFileWords delim ()
                    |> Seq.windowed (lookback + 1)
                    |> Seq.map splitLastItem
                    |> Seq.fold addToDict (PlaceCache())

let rnd = System.Random()

let pickRandomArr a = Array.get a (rnd.Next(Array.length a - 1))
let pickRandomList l = List.nth l (rnd.Next(List.length l - 1))
let pickRandomToken (l:Tokens) = l |> Seq.nth (rnd.Next(l.Count - 1))

let isUpper c = System.Char.IsUpper(c)

// pick one that starts with underscore
let pickRandomBeginning (dict:PlaceCache) = dict.Keys
                                            |> Seq.filter (fun k -> k.[0].[0] |> isUpper)
                                            |> Seq.toArray
                                            |> pickRandomArr
                                            |> List.rev

let rec generateMarkov beginPicker lenght (dict:PlaceCache) =
    let beginning = dict |> beginPicker
    let n = beginning |> List.length
    [0 .. lenght - 1 - n] // starts from -n because we add n items in one go when []
        |> List.fold (fun state i ->
                        let key = state |> Seq.take n |> List.ofSeq |> List.rev
                        let values = dict.[key]
                        pickRandomToken values :: state) beginning


let accumulateMarkov aSeq = aSeq |> List.fold (fun state x -> x + " " + state) ""

let text = createDict () |> generateMarkov pickRandomBeginning textLenght |> accumulateMarkov

printfn "%s" text

(*
// Build a map using the N - 1 items in (a1,... aN) as key and aN as value
let makeMap = Seq.map (fun t -> let n = Seq.length t
                                Seq.take (n - 1) t |> Seq.toList, Array.get t (n - 1))
              >> Seq.fold (fun map (k, v) ->
                                match map |> Map.tryFind k with
                                | None          -> map |> Map.add k [v]
                                | Some(words)   -> map |> Map.add k (v::words)) Map.empty


let createDict = readFileWords delim
                    >> Seq.windowed (lookback + 1)
                    >> makeMap

let rnd = System.Random()

let pickRandom l = List.nth l (rnd.Next(List.length l - 1))

let isUpper c = System.Char.IsUpper(c)

// pick one that starts with underscore
let pickRandomBeginning = Map.toList 
                            >> List.map fst
                            >> List.filter (fun (k: string list) -> (k |> List.head).[0] |> isUpper)
                            >> pickRandom
                            >> List.rev

let rec generateMarkov beginPicker lenght dict =
    let beginning = dict |> beginPicker
    let n = beginning |> List.length
    [0 .. lenght - 1 - n] // starts from -n because we add n items in one go when []
        |> List.fold (fun state i ->
                        let key = state |> Seq.take n |> Seq.toList |> List.rev
                        let values = dict |> Map.find key
                        pickRandom values :: state) beginning


let accumulateMarkov aSeq = aSeq |> List.fold (fun state x -> x + " " + state) ""

let text = createDict () |> generateMarkov pickRandomBeginning textLenght |> accumulateMarkov

printfn "%s" text
*)

(*
let dict = System.Collections.Generic.Dictionary<string list, System.Collections.Generic.List<string>>()

let makeMap' words =
    let l = System.Collections.Generic.List<string>()
    for i = 0 to lookback - 1 do
        l.Add "\n"

    for w in words do
        let key = l |> Seq.toList
        let found, values = dict.TryGetValue key
        if not(found)
            then
                let l =  System.Collections.Generic.List<string>()
                l.Add(w)
                dict.Add (key, l)
            else values.Add(w)

        l.RemoveAt 0
        l.Add w

        

let makeMap words =
    words
    |> Seq.map (fun t -> let n = Seq.length t
                         Seq.take (n - 1) t |> Seq.toList, Array.get t (n - 1))
    |> Seq.iter (fun (key, v) ->
                    let found, values = dict.TryGetValue key
                    if not(found)
                        then
                            let l =  System.Collections.Generic.List<string>()
                            l.Add(v)
                            dict.Add (key, l)
                        else values.Add(v) )
                    
    
let createDict = readFileWords delim
                    //>> Seq.windowed (lookback + 1)
                    >> makeMap'

let rnd = System.Random()

let getRandArrElement arr = Array.get arr (rnd.Next(arr.Length))

let isUpper c = System.Char.IsUpper(c)

// pick one that starts with underscore
let pickRandomBeginning () = dict.Keys
                                |> Seq.filter (fun (k: string list) -> (k |> List.head).[0] |> isUpper)                            
                                |> Seq.toArray
                                |> getRandArrElement

let rec generateMarkov lenght =
    let beginning = pickRandomBeginning ()
    let n = beginning |> List.length

    let res = System.Collections.Generic.List<string>()
    let queue = System.Collections.Generic.List<string>()

    for b in beginning do
        res.Add(b)
        queue.Add(b)

    for i = 0 to lenght - 1 - n do
        let key = queue |> Seq.toList
        let values = dict.[key]
        let v = values.ToArray() |> getRandArrElement
        res.Add v
        queue.Add v
        queue.RemoveAt 0
    res

createDict ()

let rndWords = generateMarkov textLenght
let mutable b = System.Text.StringBuilder()

for w in rndWords do
    b <- b.Append(w)
    b <- b.Append(" ")

printfn "%s" (b.ToString())

*)

                            

