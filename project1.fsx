#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open Akka.TestKit

type ProcessorMessage = ProcessJob of int64 * int64
type Message = 
    | START of int * int
    | DONE

let system = System.create "system" (Configuration.defaultConfig())

let sqrtOf x = x * x
let isSqrt (x: int64) (y: int64) = x * x = y

let worker = 
    spawn system "worker"
    <| fun childMailbox ->
        let rec childLoop() = 
            actor {
                let! ProcessJob(s,e) = childMailbox.Receive ()
                let sum1 = int64 (List.sumBy sqrtOf [s .. e])    
                let right = int64 (sqrt(double sum1) + 0.5)

                if isSqrt right sum1 then
                    printfn "ans: %u" s
                    
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
                let sender = bossMailbox.Sender()
                match msg with
                | START (x, y) ->
                    for i in 1 .. x do
                        let s = int64 i
                        let e = int64 (i + y - 1)
                        worker <! ProcessJob(s,e)
                        count <- count + 1
                    
                | DONE -> 
                    count <- count - 1
                    if count = 0 then 
                        printfn "DONE"
                        Environment.Exit 1
                
                return! bossLoop()
            }
        bossLoop()

let main() =
    let args = System.Environment.GetCommandLineArgs()
    let n = int args.[3]
    let k = int args.[4]
    for timeout in [1000000] do
        try
            let task = (boss <? START (n, k))
            Async.RunSynchronously (task, timeout)

        with :? TimeoutException ->
            printfn "ask: timeout!"
    0

main()