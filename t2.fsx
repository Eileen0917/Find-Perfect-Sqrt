#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open Akka.TestKit

type ProcessorMessage = ProcessJob of int * int
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
                    return! bossLoop()   
                    
                | DONE -> 
                    count <- count - 1
                    if count = 0 then 
                        printfn "DONE"
                        // bossMailbox.Sender() <! "FF"
                    else
                        return! bossLoop()   
            }

        bossLoop()



async {
    let args = System.Environment.GetCommandLineArgs()
    let n = int args.[3]
    let k = int args.[4]
    boss <! START (n, k)
    // let! response = boss <? START (n, k)
    // printfn "%s" response
} |> Async.RunSynchronously

system.Terminate()