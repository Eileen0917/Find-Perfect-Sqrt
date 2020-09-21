#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open Akka.TestKit

let system = System.create "system" (Configuration.defaultConfig())

let sqrtOf x = x * x
let isSqrt (x: int64) (y: int64) = x * x = y
let calculateSqrt i k =
    let sumOfAll = int64 (List.sumBy sqrtOf [i .. i + k - 1])
    let right = int64 (sqrt(double sumOfAll) + 0.5)
    let ans = isSqrt right sumOfAll
    if ans then printfn "ans: %A" i

let worker = 
    spawn system "worker"
    <| fun workerMailbox ->
        let rec workerLoop() = 
            actor {
                let! msg = workerMailbox.Receive ()
                match msg with
                | (x, y, z) ->
                    for i in x .. y do
                        calculateSqrt i z

                return! workerLoop()
            }
        workerLoop()

let args = System.Environment.GetCommandLineArgs()
let n = int args.[3]
let k = int args.[4]
let m = Math.Min(n/2, 1000)
let seqRange = seq { 1 .. n/m .. n }

for i in seqRange do
    worker <! (i, i + n / m - 1, k)