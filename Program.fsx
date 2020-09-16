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

let system = ActorSystem.Create("FSharp")


type ProcessorMessage = ProcessJob of int * int

let processor (mailbox: Actor<_>) = 
    let rec loop () = actor {
        let! ProcessJob(x,y) = mailbox.Receive ()
        let sq s = s * s
        let mutable sum1 = 0
        let z = 
            for i in 1 .. x do
                let mutable sum1 = 0
                for j in i .. (i+y-1) do
                    let a = sq j
                    sum1 <- sum1 + a
                    
                let right = sqrt(float sum1);
                let ans = 
                    if ((right * right) = float sum1) then i
                    else 0
                printfn "ans: %i" ans
        return! loop ()
    }
    loop ()

let processorRef = spawn system "processor" processor

processorRef <! ProcessJob(3,2)

system.Terminate()


