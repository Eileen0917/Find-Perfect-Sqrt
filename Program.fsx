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
    let rec loop () = 
        actor {
            let! ProcessJob(x,y) = mailbox.Receive ()

            for i in 1 .. x do
                let mutable sum1 = int64 0
                for j in i .. (i+y-1) do
                    let a = int64 (j * j)
                    sum1 <- sum1 + a
                    
                let right = int64 (sqrt(double sum1) + 0.5)
                if ((right * right) = sum1) then printfn "ans: %A" i
                    
            return! loop ()
        }
    loop ()

let processorRef = spawn system "processor" processor

processorRef <! ProcessJob(3,2)

system.Terminate()


