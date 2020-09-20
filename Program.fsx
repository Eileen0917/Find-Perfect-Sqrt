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

// #Using Actor
// Actors are one of Akka's concurrent models.
// An Actor is a like a thread instance with a mailbox. 
// It can be created with system.ActorOf: use receive to get a message, and <! to send a message.
// This example is an EchoServer which can receive messages then print them.

type CustomException() =
    inherit Exception()

type ProcessorMessage = ProcessJob of int * int

let mutable count = 0

let system = System.create "system" (Configuration.defaultConfig())
// create parent actor to watch over jobs delegated to it's child
let parent = 
    spawnOpt system "parent" 
        <| fun parentMailbox ->
            // define child actor
            let rec child = 
                spawn parentMailbox "child" <| fun childMailbox ->
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
                                
                            count <- count + 1
                            // childMailbox.Sender() <! "FINISHED"
                            return! childLoop()
                        }
                    childLoop()
            // define parent behavior
            let rec parentLoop() =
                actor {
                    let! ProcessJob(x,y) = parentMailbox.Receive()
                    for i in 1 .. x do
                        let s = i
                        let e = i + y - 1
                        child.Forward(ProcessJob(s,e))  // forward all messages through
                    
                    if count = x then 
                        parentMailbox.Sender() <! "DONE"
                    return! parentLoop()
                }
            parentLoop()
        // define supervision strategy
        <| [ SpawnOption.SupervisorStrategy (
                // restart on Custom Exception, default behavior on all other exception types
                Strategy.OneForOne(fun e ->
                match e with
                | :? CustomException -> Directive.Restart 
                | _ -> SupervisorStrategy.DefaultDecider.Decide(e)))  ]

async {
    let args = System.Environment.GetCommandLineArgs()
    let k = int args.[3]
    let n = int args.[4]
    let! response = parent <? ProcessJob(k,n)
    printfn "%s" response
} |> Async.RunSynchronously