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

let system = System.create "system" (Configuration.defaultConfig())
// create parent actor to watch over jobs delegated to it's child
let parent = 
    spawnOpt system "parent" 
        <| fun parentMailbox ->
            // define child actor
            let child = 
                spawn parentMailbox "child" <| fun childMailbox ->
                    childMailbox.Defer (fun () -> printfn "Child stopping")
                    printfn "Child started"
                    let rec childLoop() = 
                        actor {
                            let! ProcessJob(x,y) = childMailbox.Receive ()
                            
                            // change i from 1 .. x to use parent actor to call
                            for i in 1 .. x do
                                let mutable sum1 = int64 0
                                for j in i .. (i+y-1) do
                                    let a = int64 (j * j)
                                    sum1 <- sum1 + a
                                    
                                let right = int64 (sqrt(double sum1) + 0.5)
                                if ((right * right) = sum1) then 
                                    printfn "ans: %A, right: %A, sum1: %A" i right sum1 
                                    // TODO: and then stop but not found correct way yet
                                    // Context.Stop(child);
                            
                            return! childLoop()
                        }
                    childLoop()
            // define parent behavior
            let rec parentLoop() =
                actor {
                    let! ProcessJob(x,y) = parentMailbox.Receive()
                    child.Forward(ProcessJob(x,y))  // forward all messages through
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
    let! response = parent <? ProcessJob(3, 2)
    printfn "%s" response
    // after this one child should crash
    // parent <! Crash
    System.Threading.Thread.Sleep 200
    
    // // actor should be restarted
    // let! response = parent <? Echo "hello world2"
    // printfn "%s" response
} |> Async.RunSynchronously


// TODO: get args from command line
