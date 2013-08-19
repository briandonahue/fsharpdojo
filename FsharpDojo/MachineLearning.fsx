open System
open System.IO

type Example = { Label:int; Pixels:int[] }

let removeHeader (ary : string[][]) =
    ary.[1 .. ]

let intConvert arrayOfStrings =
    arrayOfStrings
    |> Array.map(fun (s:string) -> Convert.ToInt32(s))

let parseData fileName =
    File.ReadAllLines(fileName)
    |> Array.map(fun s -> s.Split(','))
    |> removeHeader
    |> Array.map(fun r -> intConvert r)
    |> Array.map(fun r -> {Label = r.[0]; Pixels = r.[1 .. ]})

let referenceData = parseData @"c:\development\fsharpdojo\trainingsample.csv"

let square x = x*x
let distance (P1: int[]) (P2: int[]) = 
    Array.map2 (fun p1 p2 -> square(p1 - p2)) P1 P2 |> Array.sum
    
let classify (unknown:int[]) =
  (referenceData |> Array.minBy(fun x -> distance x.Pixels unknown)).Label




let dataToTest = parseData @"c:\development\fsharpdojo\validationsample.csv"
 
let validRecords = 
    dataToTest
    |> Array.map(fun x -> (classify x.Pixels) = x.Label)
    |> Array.filter(fun x -> x)

let validity = ((float)validRecords.Length)/((float)dataToTest.Length)

printfn "Percentage: %f" validity

let invalid = 
    dataToTest 
    |> Array.map (fun x -> x.Label, (classify x.Pixels))
    |> Array.filter(fun (real, predicted) -> real <> predicted)
    |> Array.iter (fun (real, predicted) -> printfn "%i %i" real predicted) 