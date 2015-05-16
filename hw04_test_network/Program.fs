// Task 32 Tests of simulation of computer network with virus
// by Vladimir Yumatov
// SPBU, group 171

// Expected execution time: 2.5 h
// Real time: 1.5 h

open NUnit.Framework
open FsUnit

type Comp (sysname : string, inf : bool) = 
  class
    let mutable infected = inf
    let InfProb sys = 
      match sys with
      | "Shindows" -> 0.52
      | "Linuh"    -> 0.267
      | "OS Pex"   -> 0.29
      | "Vedroid"  -> 0.371
      | _ -> failwith "Really?"

    member this.tryInfect (r : System.Random) =
      if (r.NextDouble() < (InfProb sysname)) then 
        infected <- true
    member this.isInfected () = infected
    member this.OS = sysname

    override this.ToString () = 
      (if infected then "Infected" else "Not infected") + "\t" + sysname
  end


type Network(complist : Comp list, connects : (int * int) list, 
             rnd : System.Random) = 
  class
    let clist = complist
    let size = clist.Length
    let edges = Array.create size List.empty
    let mutable infnum = 0


    do 
      for (n1, n2) in connects do
        Array.set edges n1 (n2::edges.[n1])
        Array.set edges n2 (n1::edges.[n2])


    member this.P () = printfn "%A" edges
    member this.NextStep () = 
      let inflist = Array.filter (fun k -> clist.[k].isInfected()) 
                                  [|0..size-1|]
      for i in inflist do
        for j in edges.[i] do
          clist.[j].tryInfect(rnd)
   
    member this.Markers () = 
      List.map (fun (c : Comp) -> if c.isInfected() then "!" else " ") 
        clist

    member this.Status () = 
      for i in clist do
        printfn "%A" i
  end


type Rand (pr : float) = 
  inherit System.Random()
  override this.NextDouble() = pr


[<TestFixture>]
type ``Network without infection spreading`` () =
   let nw = Network([Comp("Shindows", false); Comp("Vedroid", false); 
                    Comp("OS Pex", true); Comp("Linuh", false)], 
                    [(0, 1); (1, 2); (2, 3)], Rand(1.0))
   

   [<TestCase(1,   Result = "S -- V -- P!-- L ", 
                   TestName = "1 step")>]
   [<TestCase(100, Result = "S -- V -- P!-- L ", 
                   TestName = "100 steps")>]
   member this.``Nobody get infected in`` snum = 
     for i in 1..snum do nw.NextStep()
     let ims = nw.Markers()
     "S"+ims.[0]+"-- V"+ims.[1]+"-- P"+ims.[2]+"-- L"+ims.[3]



[<TestFixture>]
type ``Network with 100% probability of infection spreading`` () =

   [<TestCase(1, Result = "L -- S -- V!-- P!-- L!", 
                 TestName = "1 step")>]
   [<TestCase(2, Result = "L -- S!-- V!-- P!-- L!", 
                 TestName = "2 steps")>]
   [<TestCase(3, Result = "L!-- S!-- V!-- P!-- L!", 
                 TestName = "3 steps")>]
   member this.``Get infected in`` snum = 
     let net = Network([Comp("Linuh", false);   Comp("Shindows", false); 
                        Comp("Vedroid", false); Comp("OS Pex", true); 
                        Comp("Linuh", false)], 
                       [(0, 1); (1, 2); (2, 3); (3, 4)], Rand(0.0))
     for i in 1..snum do net.NextStep()
     let ims = net.Markers()
     "L"+ims.[0]+"-- S"+ims.[1]+"-- V"+ims.[2]+
     "-- P"+ims.[3]+"-- L"+ims.[4]



[<TestFixture>]
type ``Network without connections`` () =
 
   [<TestCase(1,   Result = "L!   S    V    P!   L ", 
                   TestName = "1 step")>]
   [<TestCase(100, Result = "L!   S    V    P!   L ", 
                   TestName = "100 steps")>]
   member this.``Nobody get infected in`` snum = 
     let net = Network([Comp("Linuh", true);   Comp("Shindows", false); 
                        Comp("Vedroid", false); Comp("OS Pex", true); 
                        Comp("Linuh", false)], 
                       [], Rand(0.0))
     for i in 1..snum do net.NextStep()
     let ims = net.Markers()
     "L"+ims.[0]+"   S"+ims.[1]+"   V"+ims.[2]+
     "   P"+ims.[3]+"   L"+ims.[4]


[<EntryPoint>]
let main argv = 
  let nw = Network([Comp("Linuh", false);   Comp("Shindows", false); 
                     Comp("Vedroid", false); Comp("OS Pex", true); 
                     Comp("Linuh", false)], 
                    [(0, 1); (1, 2); (2, 3); (3, 4)], Rand(0.0))
  nw.P()
 
  0 
    