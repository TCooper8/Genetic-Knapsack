
open System

let rand = new Random()

type Gene = 
    | A
    | B
    | C
    | D
    | E
    | F 
    | G 
    | H 
    | I
   
let geneArray = [| A; B; C; D; E; F; G; H; I |]
let geneEnum i = 
    Array.get geneArray (i % geneArray.Length)

type 'a GeneFunction = Gene -> 'a

type Allele = 
    | Dominant of Gene list
    | Recessive of Gene list

type GeneStrand = Gene list

let geneFunc gene =
    match gene with
    | A -> 5.25
    | B -> 1.50
    | C -> 7.25
    | D -> 3.25
    | E -> 4.25
    | F -> 3.10
    | G -> 2.80
    | H -> 1.90
    | I -> 3.0

let genStrand len =
    [ for i in 1..len do yield geneEnum (rand.Next()) ]

let rec crossOver xs ys acc =
    match (xs, ys) with
    | ([], []) -> acc
    | ([], ys) -> ys@acc
    | (xs, []) -> xs@acc
    | (x::xs, y::ys) -> 
        if rand.Next() % 2 = 0 then
            crossOver xs ys (y::acc)
        else crossOver xs ys (x::acc)

let rate strand =
    let rating = List.map geneFunc strand |> List.sum
    printfn "Rating %A with %A" strand rating |> ignore
    rating

let searchIterative len itters goal =
    let rec loop i res resR =
        let a = genStrand len
        let c = crossOver a res []

        let cR = rate c

        if cR = goal then Some c, i
        else if i >= itters then None, i
        else 
            let dC = goal - cR
            let dRes = goal - resR
            if dC < dRes then 
                loop (i+1) c cR
            else loop (i+1) res resR
    loop 0 (genStrand len) 0.

[<EntryPoint>]
let main argv = 
    let strandLength = 15
    let itters = 1000000
    let goal = 60.50

    let res = searchIterative strandLength itters goal

    match res with
    | Some(strand), i ->
        printfn "Strand %A with %A rating" strand (List.map geneFunc strand |> List.sum) |> ignore
        printfn "%i itterations" i |> ignore
    | None, i ->
        printfn "No strand found." 
        printfn "%i itterations" i |> ignore

    0 // return an integer exit code

