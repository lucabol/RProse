(**
== Background
I like reading old programming books. I was reading
http://www.amazon.com/Practice-Programming-Addison-Wesley-Professional-Computing/dp/020161586X[The practice of programming] 
 (from the 80s) when I stumbled into a nice program to generate gibberish text. An example of it, from the bible,
 is below:

Israel: put it also is with thy rivers, and over the asses of Zibeon the Hivite; 36:3 And if the thief be found naked. 5:4 For we know that the LORD concerning Jehoiakim the king, except the king may
judge them; then he will take pleasure in the twelfth year, in the brooks of honey and the elders of that place Beersheba; because there is no light in the Jews' enemy. 3:11 And the border were at Bet
hel came forth of my teachers, nor inclined their ear, but walked in the ditch, and mine eyes and mine arm fall from my face, except your brother be waxen poor, and ye visited me: I was astonished at
the time was come near to her own works praise her in an earthen vessel over running water: 14:7 And there arose up a parable unto us, and brought him to the young men are peaceable with us; there sha
ll they that conspired against him, and covered the tabernacle, and it shall be as still as a wild beast of the gold become dim! how is she without, now in thine hand, and in the cities that be with y
ou alway, even unto the coming of the LORD pondereth the hearts.

The authors wrote the algorithm in several programming languages and then tested both their performance and how many
lines of code it took to write each one. I decided to bring the example to the modern era.

I grabbed the code from http://cm.bell-labs.com/cm/cs/tpop/code.html[here], made it compile, wrote a perf test
and rewrote it in F# for comparison. Sneak preview below ...

[width="40%", frame="none", options="header", grid="rows", cols="<h, >, >", valign="bottom"]
|================================
|           | KJ Bible  | PSalms
| C         | 2.51      | 0.96
| C++       | 5.13      | 1.72
| Awk       | 4.56      | 1.22
| Perl      | 2.78      | 0.90
| F# (map)  |11.42      | 1.63
| F# (hash) | 2.68      | 1.03
| F# (imper)| 2.71      | 0.93
|=================================

The algorithm takes a text and divides it into words. It uses the first N words as the start state.
It then looks in the text for all the words following these N words and picks one at random. It then advances
the N words and repeats.

It would be odiously inefficient to implement it as such. You really want to do two steps: one to
gather all the prefixes and the other to generate the text. Let's see.
**)

(**
== The code
All of the below values should be parameters to the program ...
**)

open System.Collections.Generic
open System.Text

let lookback    = 2
let textLenght  = 10000 // words
let delim       = [|' '; '\n'; '\t';'\r'|]

(** The following code splits stdin into words **)
let split d (s: string) = s.Split(d)
let readFileWords delim = System.Console.In.ReadToEnd >> split delim >> Seq.filter (fun l -> l <> "")

(**
This is the most interesting part of the code: which data structure to choose to store the prefixes. I did it with
a +map+ first, which causes the large times fo _F# (map)_ that you see above. I then moved to a +Dictionary+, gaining
most of the performance back. Subsequently I also used a mutable list instead of an immutable one as Tokens,
bringing down the times at par with the C implementation.

The reasons for the perf improvements are:
. +Dictionary+ is an hashtable while +map+ is a binary tree. Inserting and looking up is then O(1) instead of O(long n).
. Using a mutable list allows us not to have to remove a list from the hashtable and then add it again, avoiding one lookup and one insert
**)

type Keys = string list
type Tokens = List<string>
type PlaceCache = Dictionary<Keys, Tokens>

let addToken t (l:Tokens) = l.Add(t); l

let addToDict (dict:PlaceCache) (k, v) =
    let found, values = dict.TryGetValue(k)
    if found then addToken v values |> ignore
             else dict.Add (k, addToken v (Tokens()))
    dict

(**
Build an hash table using the N - 1 items in (a1,... aN) as key and aN as value. I have the feeling that
+Seq.windowed+ is on the expensive side and could be optimized. But I hit my perf target so I didn't optimize
further.
**)

let splitLastItem a = let n = Array.length a
                      Array.sub a 0 (n - 1) |> Array.toList, Array.get a (n - 1)

let createDict () = readFileWords delim ()
                    |> Seq.windowed (lookback + 1)
                    |> Seq.map splitLastItem
                    |> Seq.fold addToDict (PlaceCache(10000))

(** Generic way to pick random elements from an array and list **)
let rnd = System.Random()
let pickRandomArr a = Array.get a (rnd.Next(Array.length a - 1))
let pickRandomToken (l:Tokens) = l |> Seq.nth (rnd.Next(l.Count - 1))

(** Pick a prefix that starts with uppercase by looking at all the prefixes **)
let pickRandomBeginning (dict:PlaceCache) = dict.Keys
                                            |> Seq.filter (fun k -> k.[0].[0] |> System.Char.IsUpper)
                                            |> Seq.toArray
                                            |> pickRandomArr
                                            |> List.rev

(** This just implements the algorithm described above. It generates N gibberish words.
Again, I have the suspicion that <1> below could be optimized further, but didn't investigate.
**)
let rec generateMarkov beginPicker lenght (dict:PlaceCache) =
    let beginning = dict |> beginPicker
    let n = beginning |> List.length
    [0 .. lenght - 1 - n] // starts from -n because we add n items in one go when []
        |> List.fold (fun state i ->
                        let key = state |> Seq.take n |> List.ofSeq |> List.rev // <1>
                        let values = dict.[key]
                        pickRandomToken values :: state) beginning


(** This just accumulate the string. A +StringBuilder+ would be faster,
but it is negligeble compared to the time spent in the tight loops above.
**)
let accumulateMarkov aSeq = aSeq |> List.fold (fun state x -> x + " " + state) ""

let text = createDict () |> generateMarkov pickRandomBeginning textLenght |> accumulateMarkov

printfn "%s" text

(**
== Comments
If you look at the performance table above, some things are striking:
. There got to be something wrong with the C++ implementation, it can't be so bad
. Perl is very fast for this kind of processing (albeit Perl and Awk are specific to 2 words prefixes)
. Awk is taken from https://github.com/danfuzz/one-true-awk[here] and running on Windows. A better Awk will do better.
. F#, properly optimized, is a fierce competitor in this scenario
. It is unfortunate that you have to get out of the functional paradigm to properly optimize it. But that
  exactly why F# is so powerful. You can do it.
**)

(**
== Other code
Below is the commented out code for the 'purely functional' version, using +map+, and the messy imperative one.
**)

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

                            

