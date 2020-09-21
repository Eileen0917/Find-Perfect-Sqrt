#r "/Users/hsiang-yuanliao/.nuget/packages/akka/1.4.10/lib/netstandard2.0/Akka.dll"
#r "/Users/hsiang-yuanliao/.nuget/packages/akka.fsharp/1.4.10/lib/netstandard2.0/Akka.FSharp.dll"
#r "/Users/hsiang-yuanliao/.nuget/packages/newtonsoft.json/12.0.3/lib/net45/Newtonsoft.Json.dll"
#r "/Users/hsiang-yuanliao/.nuget/packages/fspickler/5.3.2/lib/net45/FsPickler.dll"

#time "on"
open System
open Akka
open Akka.FSharp


type ProcessorMessage = InputMsg of uint64 * uint64
type ProcessorMessage2 = InputMsg2 of uint64 * uint64 * uint64

let SumConsecSquare a b = 
    (( ( a + b - 1UL ) * ( a + b ) * ( 2UL * a + 2UL * b - 1UL ) ) / 6UL) - (( ( a - 1UL ) * a * ( 2UL * a - 1UL) ) / 6UL)
    
let isPerfectSquare x =
    let h = x &&& 0xFUL
    if (h > 9UL) then false
            else
              if ( h <> 2UL && h <> 3UL && h <> 5UL && h <> 6UL && h <> 7UL && h <> 8UL ) then
                let t = ((x |> double |> sqrt) + 0.5) |> floor|> uint64
                t*t = x
              else false
             


let a = uint64 fsi.CommandLineArgs.[1]
let b = uint64 fsi.CommandLineArgs.[2]

//let a = 3UL
//let b = 2UL
let system = System.create "system" <| Configuration.defaultConfig()

let spawnChild childActor name =
    spawn system name childActor
let sendMsg tActor (InputMsg ( x , y))= 
    tActor <! InputMsg(x, y)
let sendMsg2 tActor (InputMsg2 (x, y, z))= 
    tActor <! InputMsg2(x, y, z)

let sendStrMsg tActor (str:string) =
    tActor <! str



let myActor (mailbox: Actor<_>) =
    let rec loop() = actor {
        //let! InputMsg(n, k) = mailbox.Receive()
        let! InputMsg2(head, tail, k) = mailbox.Receive()

        //printfn "Worker Actor: Received inputs head=%u, tail=%u, k=%u" head tail k
        if head = 0UL && tail = 0UL && k = 0UL then
            //printfn "Initial done with k=%u" k
            sendStrMsg (mailbox.Sender()) "Done, ask for more jobs"
            return! loop()
   
       
        
        for i in int head .. int tail do
            
            if(isPerfectSquare(SumConsecSquare (uint64 i) k)) then
                printfn "%d" i 
        //printfn "Worker Actor: Received inputs head=%u, tail=%u, k=%u" head tail k
            //sendStrMsg (mailbox.Sender()) "Done, ask for more jobs"

   
        
        sendStrMsg (mailbox.Sender()) "Done, ask for more jobs"
        return! loop()

    }
    loop()
    

let myActor2 (mailbox: Actor<_>) =
    let rec loop() = actor {
        //let! InputMsg(n, k) = mailbox.Receive()
        let! InputMsg(n, k) = mailbox.Receive()
        //printfn "processor-1: Received inputs n=%u, k=%u" n k
        
        if(isPerfectSquare(SumConsecSquare (uint64 n) k)) then
                printfn "%u" n
        return! loop()
    }
    loop()


let myBossActor (mailbox: Actor<_>) =
    let rec loop() = actor {
        let! InputMsg(n, k) = mailbox.Receive()
        //let part1 = n/2UL
        //let part2 = part1 + 1UL
        let portions = 
            if n < 100UL then
                n
            else
                n / uint64 100;
        let bestP = ((n |> double)/double portions) + 0.5|> floor |> uint64
        printfn "bestP: %u" bestP

        let head x =  
                n / bestP * x + 1UL

        let tail y =  
            if y = bestP then
              n 
            else
              n / bestP * y

        // just seperate the original problem to several subproblems
        for i in 0 .. int bestP-1 do
            //printfn "%u %u" (head (uint64 i)) (tail (uint64 i+1UL))
            //printf ":"
            sendMsg2 (spawnChild myActor (string i)) (InputMsg2(head (uint64 i), tail (uint64 i+1UL), k))



        //sendMsg2 (spawnChild myActor (string i)) (InputMsg2(n/4UL*3UL+1UL,n, k))

        
        //real time 3.7s 
        // n actors
        (*
        for i in 1 .. int n do
            sendMsg (spawnChild myActor2 (string i)) (InputMsg(uint64 i, k))
        *)

        
        //real time 3.7s
        // 4 actors
        (*
        sendMsg2 (spawnChild myActor (string 1)) (InputMsg2(1UL, n/4UL, k))
        sendMsg2 (spawnChild myActor (string 2)) (InputMsg2(n/4UL+1UL, n/2UL, k))
        sendMsg2 (spawnChild myActor (string 3)) (InputMsg2(n/2UL+1UL, n/4UL*3UL, k))
        sendMsg2 (spawnChild myActor (string 4)) (InputMsg2(n/4UL*3UL+1UL,n, k))
        *)


        
        // 1 actor
        //for i in 1 .. int n do
        //sendMsg2 (spawnChild myActor (string 2)) (InputMsg2(1UL, n, k))

        return! loop()
    }
    loop()

let myBossActorRef = spawn system "Boss-Processor" myBossActor

//sendMsg myBossActorRef (InputMsg( a , b))
    

let myBossActor2 paramN paramK (mailbox: Actor<_>) =
    let rec loop lastParamN paramK= actor {
        let! message = mailbox.Receive()
        
        //All the jobs are done
        if lastParamN = 0 then
            return! loop 0 0

        //We need to distribute the jobs
        //1 job for 1 actor first (TBD)
        //sendMsg2 (mailbox.Sender()) (InputMsg2(uint64 lastParamN, uint64 lastParamN, uint64 paramK))
        
        //n jobs for 1 actor:
        let n = 1000
        
        if lastParamN <= n then
            sendMsg2 (mailbox.Sender()) (InputMsg2(uint64 1, uint64 lastParamN, uint64 paramK)) 
            return! loop 0 paramK
        else
            sendMsg2 (mailbox.Sender()) (InputMsg2(uint64 (lastParamN-n+1), uint64 lastParamN, uint64 paramK))
            
            let newParamN = lastParamN - n
            //printfn "Updated N = %u, k = %u" newParamN paramK
            return! loop newParamN paramK

        


        //return! loop newParamN paramK
    }
    //printfn "Initail N = %u, k = %u" paramN paramK
    //TODO: Create Worker Actor here
    // how many worker actors? Let's create 4 here first
    //send the head tial and k to 0 first
    // We need at least 8 actors and 1000 jobs everytime
    // Or... we will get some error when n is big
    for i in 1 .. 8 do
        sendMsg2 (spawnChild myActor (string i))  (InputMsg2(0UL, 0UL, 0UL))
 
    loop paramN paramK

let myBossActorRef2 = spawn system "Boss-Processor-2" (myBossActor2 (int a) (int b))
//sendStrMsg myBossActorRef2 "Done msg"

