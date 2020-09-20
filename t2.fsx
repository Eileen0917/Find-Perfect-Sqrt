// ActorSayHello.fsx
#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 
// #load "Bootstrap.fsx"

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open Akka.TestKit

type CustomException() =
    inherit Exception()

type ProcessorMessage = ProcessJob of int * int
type Message = 
    | START of int * int
    | DONE



let system = System.create "system" (Configuration.defaultConfig())

let worker = 
    spawn system "worker"
    <| fun childMailbox ->
        let rec childLoop() = 
            actor {
                let! ProcessJob(s,e) = childMailbox.Receive ()
                
                let mutable sum1 = int64 0
                for j in s .. e do
                    let a = int64 (j * j)
                    sum1 <- sum1 + a
                    
                let right = int64 (sqrt(double sum1) + 0.5)
                if ((right * right) = sum1) then 
                    printfn "ans: %A" s
                    
                childMailbox.Sender() <! DONE
                return! childLoop()
            }
        childLoop()

let boss = 
    spawn system "boss" 
    <| fun bossMailbox ->
        let mutable count = 0
        let rec bossLoop() =
            actor {
                let! (msg: Message) = bossMailbox.Receive()
                match msg with
                | START (x, y) ->
                    for i in 1 .. x do
                        let s = i
                        let e = i + y - 1
                        worker <! ProcessJob(s,e)
                        count <- count + 1
                | DONE -> 
                    
                    count <- count - 1
                    if count = 0 then 
                        printfn "need a way to exit"
                        // bossMailbox.Sender <! "FINISH"
                
                return! bossLoop()
            }
        bossLoop()



async {
    let args = System.Environment.GetCommandLineArgs()
    let k = int args.[3]
    let n = int args.[4]
    let! response = boss <? START (k, n)
    // boss <! DONE
    printfn "%s" response
} |> Async.RunSynchronously

