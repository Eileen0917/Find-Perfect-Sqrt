#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 
// #load "Bootstrap.fsx"

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open Akka.TestKit

let system = System.create "system" (Configuration.defaultConfig())

let squareOf x =
    x*x

let isSquare (x : float) =
    x - floor( x ) = 0.0


let perfectSquare x k = 
    let sumOfSquares = List.sumBy squareOf [x .. x + k - 1]
    let result = 
        isSquare (sqrt( float sumOfSquares ))
    if result then
        printfn "%d" x

let traverseRange startIndex endIndex k =
    for i = startIndex to endIndex do
        perfectSquare i k

let printRange startIndex endIndex k =
    printfn "Reached"
    for i in startIndex .. endIndex do
        printfn "%d" i

let findPSquares = spawn system "findPSquares" <| fun mailbox ->
    let rec loop() = actor {
        let! msg = mailbox.Receive()
        match msg with
        | (a, b, c) -> traverseRange a b c
        return! loop()
    }
    loop()

let N = 3
let k = 2
let m = Math.Min(N/2, 1000) //number of actors, generically took N/2

let seqRange = seq { 1 .. N/m .. N }

for i in seqRange do
    findPSquares <! (i, i + N / m - 1, k)