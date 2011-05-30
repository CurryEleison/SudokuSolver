// Learn more about F# at http://fsharp.net

// We set up a 9x9 array. Each element contains a list of numbers. If there is only a single number we know that is the one
// for that element. If there are several numbers those represent the options known to be possible at that place. If there
// are no numbers in a given square we know the setup to be impossible.

// TODO: IF the sudoku isn't straightforward do a check to try random stuff
open System
open System.IO
open System.Text

type sdkelt =
     | Certain of int32
     | Options of List<int32>
     | Nothing

// Read in the input
// Accept basically anything: Either a 9x9 square or a long list of 81 numbers
let file = @"..\..\test.sdk"

let lines = File.ReadLines(file)
let trimmedlines = lines |> Seq.map(fun x -> x.Replace(".", "0").Trim())
let longline = trimmedlines |> Seq.fold(fun acc l -> String.Format("{0}{1}", acc, l)) ""
let numbers = longline.ToCharArray() |> Array.map(fun x -> 
                                                    let ci = Int32.Parse(x.ToString())
                                                    if ci = 0 then sdkelt.Options [ 1 .. 9 ]
                                                    else sdkelt.Certain ci

                                                   )

// We keep our stuff in this. Doing a 2D array was a bad idea
let elems : sdkelt[,] = Array2D.init 9 9 (fun i j -> sdkelt.Options [])

numbers |> Array.iteri(fun n x ->
    let i = n / 9
    let j = n % 9
    elems.[i, j] <- x
    )

// Can't do fold etc over  2d array so this flattens it
let flatten (A:'a[,]) = A |> Seq.cast<'a>



// Count how many certains are in the square
let countcertains (arr:sdkelt[,]) =
    flatten arr |> Seq.filter( fun x -> match x with
                                            | Certain i -> true
                                            | _ -> false ) |> Seq.length
// Check if we have contradictions
let iscontradiction (arr:sdkelt[,]) =
    let imps = flatten arr |> Seq.filter( fun x -> match x with
                                                    | Nothing -> true
                                                    | _ -> false ) |> Seq.length
    match imps with
        | 0 -> false
        | _ -> true

// Find the (i,j) of the element we want to guess at. Should rewrite this one I guess
let getelementtoguessat (arr:sdkelt[,]) =
    let mutable numelts = 9
    let mutable ibest = 0
    let mutable jbest = 0
    for i in 0 .. 8 do
        for j in 0 .. 8 do
            match arr.[i,j] with
                | Options opt -> if opt.Length < numelts then
                                            numelts <- opt.Length
                                            ibest <- i
                                            jbest <- j
                | _ -> ignore 0
    (ibest, jbest)

// Find a list of the values that can definitely not go at position i,j
let findimpossibles (arr:sdkelt[,]) i j =
    let rowimp = seq { for k in 0 .. 8 do 
                            if k <> i then 
                                match arr.[k , j] with
                                        | Certain x -> yield x
                                        | _ -> ignore 0
                        } |> Seq.toList
    let colimp = seq { for k in 0 .. 8 do 
                            if k <> j then 
                                match arr.[i, k] with
                                        | Certain x -> yield x
                                        | _ -> ignore 0
                        } |> Seq.toList
    let sqrimp = seq { for k in (i / 3) * 3 .. (i / 3) * 3 + 2 do
        for l in (j / 3) * 3 .. (j / 3 ) * 3 + 2 do
            if  not ( k = i && l = j) then 
                match arr.[k, l] with 
                    | Certain x -> yield x
                    | _ -> ignore 0
        } 
    let sqrlst = sqrimp    |> Seq.toList
    [rowimp; colimp; sqrlst ] |> List.concat

// Eliminate the obviously impossible values based on the certains we have
let rec eleminateimpossibles arr : sdkelt[,] =
    let countcertainsbefore = countcertains arr
    let res = arr |> Array2D.mapi(fun i j (el:sdkelt) -> 
            let impos = findimpossibles arr i j
            let res = match arr.[i, j] with
                        | Nothing -> sdkelt.Nothing
                        | Certain x -> if (impos |> Seq.exists(fun y -> x = y)) then sdkelt.Nothing else Certain x
                        | Options opt -> 
                                let take, leave = opt |> List.partition(fun x -> 
                                    (impos |> Seq.tryFindIndex(fun y -> x = y))  = None
                                )
                                let r = 
                                    if List.length(take) > 1 then Options take 
                                    elif List.length(take) = 1 then Certain (List.head(take))
                                    else Nothing
                                r
            res
    )
    if countcertainsbefore = (countcertains res) then res
    else eleminateimpossibles res

// Check if we are done
let issolved  (arr:sdkelt[,]) =
    (countcertains (eleminateimpossibles arr) = 9 * 9)


// Find "the" solution. Alternately elmininate obviously impossibles and guess when you can't do obvious eliminations
// I only find the first solution if there are multiple posibilites
let findsolution (arr:sdkelt[,]) =
    let f (arr:sdkelt[,]) i j =
        let s = match arr.[i,j] with 
                | Options opt -> opt  |> List.map(fun x -> 
                                    let cp = arr |> Array2D.copy
                                    cp.[i, j] <- sdkelt.Certain x
                                    eleminateimpossibles cp)
                | Certain x -> [ arr ]
                | Nothing -> []
        s |> List.filter(fun a -> not (iscontradiction a))
    let rec g (se:seq<sdkelt[,]>) =
        seq {
            for x in se do
                if issolved x then yield x
                else if iscontradiction x then yield! ([] |> List.toSeq)
                else 
                    let (i, j) = getelementtoguessat x
                    let expand = f x i j
                    yield! g expand
        }
    let init = [ (eleminateimpossibles arr) ] |> List.toSeq
    (g init) |> Seq.take(1)

// Print out the solution as a long string of numbers
// I use http://www.sudoku-solutions.com/ to check the results
let converttostring (arr:sdkelt[,]) =
    let s:StringBuilder = StringBuilder()
    arr |> Array2D.iter(fun x -> printf "%A" (match  x with 
                                                | Certain i -> i
                                                | Options opt -> opt.Head
                                                | Nothing -> 0
                                                ))
    ""

// All set up. Let's go find a solution then
printfn "--"
(findsolution elems) |> Seq.iter (fun x -> printfn "%A" (converttostring x)) 
//printfn "--"
//(findsolution (eleminateimpossibles elems)) |> Seq.iter (fun x -> printfn "%A" x) 

//Pause with the solution on screen
Console.ReadLine()