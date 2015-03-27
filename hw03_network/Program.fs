// Task 26 Simulation of computer network with virus
// by Vladimir Yumatov
// SPBU, group 171

// Expected execution time: 2.5 h
// Real time: 1.5 h

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

    member this.tryInfect () =
      if (System.Random().NextDouble() < (InfProb sysname)) then 
        infected <- true
    member this.isInfected () = infected
    member this.OS = sysname

    override this.ToString () = 
      (if infected then "Infected" else "Not infected") + "\t" + sysname
  end


type Network(complist : Comp list, connects : (int * int) list) = 
  class
    let clist = complist
    let size = clist.Length
    let edges = Array.create size List.empty
    let mutable infnum = 0


    do 
      for (n1, n2) in connects do
        Array.set edges n1 (n2::edges.[n1])
        Array.set edges n2 (n1::edges.[n2])

    member this.NextStep () = 
      let inflist = Array.filter (fun k -> clist.[k].isInfected()) 
                                  [|0..size-1|]
      for i in inflist do
        for j in edges.[i] do
          clist.[j].tryInfect()
   
    member this.Status () = 
      for i in clist do
        printfn "%A" i
  end


 

[<EntryPoint>]
let main argv = 
  let comps = 
    [Comp("Shindows", false); Comp("Linuh", true); Comp("OS Pex", false); 
     Comp("Linuh", false); Comp("Linuh", false); Comp("Vedroid", false);
     Comp("OS Pex", false); Comp("Shindows", false); Comp("Shindows", true);
     Comp("Vedroid", false)]
  let conns = [ (0, 4); (1, 2); (1, 0); (2, 4); (2, 3); (4, 5);
                (6, 7); (6, 8); (7, 9); (8, 9) ]
  let lnw = new Network(comps, conns)
  printfn 
    "Start: 
     1!-- 0           6 -- 7
     |    |           |    |
     2 -- 4 -- 5      8!-- 9
     |
     3"
  lnw.Status()


  let mutable flag = false
  printfn "\n\nNext step: push \"N\"\nExit: push anything"
  while not(flag) do
    match System.Console.ReadKey().Key with
    | System.ConsoleKey.N ->
      printfn "\n"
      lnw.NextStep()
      lnw.Status()
    | _ -> flag <- true
  0 
