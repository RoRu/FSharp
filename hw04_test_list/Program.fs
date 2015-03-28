// Tasks 33-34
// Solution for tests on ADT and array lists (tasks 28-29)
// by Vladimir Yumatov
// SPBU, group 171

// Expected execution time: 2.5 h
// Real time: 4 h

open NUnit.Framework
open FsUnit

type IPList<'A when 'A : equality> = 
  interface
    abstract Head     : unit         -> Option<'A>
    abstract Tail     : unit         -> Option<IPList<'A>>
    abstract Length   : unit         -> int
    abstract AddForw  : 'A           -> unit
    abstract AddBack  : 'A           -> unit
    abstract AddByInd : 'A           -> int  -> bool
    abstract DelFirst : unit         -> bool
    abstract DelLast  : unit         -> bool
    abstract DelByInd : int          -> bool
    abstract tryFind  : ('A -> bool) -> Option<'A>
    abstract Append   : IPList<'A>   -> unit
    abstract Print    : unit         -> unit
  end


type Data<'A> = 
  | Empty
  | Con of 'A * Data<'A>


// Task 28 List based on ADT
type ListADT<'A when 'A : equality> (basicdata : Data<'A>, basicsize : int) = 
  class
    let mutable currdata = basicdata
    let mutable currsize = 
      if basicsize < 0 then 0 
      else basicsize

    new() = ListADT(Empty, 0)
    new(h : 'A) = ListADT(Con(h, Empty), 1)

    interface IPList<'A> with
      member this.Head () = 
        match currdata with
        | Empty -> None
        | Con(h, _) -> Some h
      member this.Tail () = 
        match currdata with
        | Empty -> None
        | Con(h, Empty) -> Some (ListADT<'A>() :> IPList<'A>)
        | Con(_, t) -> Some (ListADT<'A>(t, currsize - 1) :> IPList<'A>)
      member this.Length () = currsize
      member this.AddForw a = 
        currdata <- Con(a, currdata)
        currsize <- currsize + 1
      member this.AddBack a = 
        let rec add dt elm = 
          match dt with
          | Empty -> Con(elm, Empty)
          | Con(h, t) -> Con(h, add t elm)
        currdata <- add currdata a
        currsize <- currsize + 1
      member this.AddByInd a ind = 
        if (ind < 0) || (ind > currsize) then 
          false
        else
          if ind = currsize then (this :> IPList<'A>).AddBack a; true
          else
            let rec add ato elm i = 
              match ato with 
              | Empty -> Con(elm, Empty)
              | Con(h, t) -> 
                if i < ind then
                  Con(h, add t elm (i + 1))
                else
                  Con(elm, ato)
            currdata <- add currdata a 0
            currsize <- currsize + 1
            true
      member this.DelFirst () = 
        match currdata with
        | Empty -> false
        | Con(h, t) -> 
          currdata <- t
          currsize <- currsize - 1
          true
      member this.DelLast () = 
        if currdata = Empty then false
        else  
          let rec del dt = 
            match dt with
            | Empty -> Empty
            | Con(h, Con(_, Empty)) -> Con(h, Empty)
            | Con(h, t) -> Con(h, del t)
          currdata <- del currdata
          currsize <- currsize - 1
          true
      member this.DelByInd ind = 
        if currdata = Empty then false 
        elif (ind < 0) || (ind > currsize - 1) then false
        elif ind = 0 then (this :> IPList<'A>).DelFirst()
        else
          let rec del dt i = 
            match dt with
            | Con(h, Con(h', t')) ->
              if (i = ind - 1) then 
                Con(h, t')
              else 
                Con(h, del (Con(h', t')) (i + 1))
            | _ -> Empty
          currdata <- del currdata 0
          currsize <- currsize - 1
          true
      member this.tryFind f = 
        let rec find dt = 
          match dt with
          | Empty -> None
          | Con(h, t) ->
            if f h then Some h
            else find t
        find currdata
      member this.Append tail =
        let rec app (ato : IPList<'A>) (lst : IPList<'A>) = 
          match lst.Length() with
          | 0 -> ()
          | _ -> 
            ato.AddBack (lst.Head().Value)
            app ato (lst.Tail().Value)
        app this tail

      member this.Print () = ()

    override this.ToString () =  
      let rec tos dt = 
        match dt with
        | Empty -> "|)"
        | Con(h, Empty) -> h.ToString() + "|)"
        | Con(h, t) -> h.ToString() + "; " + tos t
      "(|" + tos currdata
  end



[<TestFixture>]
type ``ADT list test`` () = 
  [<Test>]
  member this.``AddForward`` () = 
    let lst = ListADT(0) :> IPList<int>
    lst.AddForw 1
    (lst.ToString()) |> should equal "(|1; 0|)"
  
  [<Test>]
  member this.``AddBack`` () = 
    let lst = ListADT(0) :> IPList<int>
    lst.AddBack 1
    (lst.ToString()) |> should equal "(|0; 1|)"
  
  [<TestCase(5, 3, Result = "(|1; 0; 2; 5|)",
                   TestName = "Add to tail")>]
  [<TestCase(8, 0, Result = "(|8; 1; 0; 2|)",
                   TestName = "Add head")>]
  [<TestCase(3, 2, Result = "(|1; 0; 3; 2|)",
                   TestName = "Add to middle")>]
  [<TestCase(3, 21, Result = "(|1; 0; 2|)",
                   TestName = "Index out of range")>]
  member this.``AddByIndex`` a i = 
    let lst = ListADT(0) :> IPList<int>
    lst.AddForw 1
    lst.AddBack 2
    lst.AddByInd a i |> ignore
    lst.ToString()

  [<Test>]
  member this.``DelFirst (Valid; Empty list)`` () = 
    let lst = ListADT<int>(Con(1, Con(2, Con(3, Empty))), 3) :> IPList<int>
    lst.DelFirst() |> ignore
    (lst.ToString()) |> should equal "(|2; 3|)"
    let lst = ListADT() :> IPList<int>
    lst.DelFirst() |> should equal false
    
  [<Test>]
  member this.``DelLast (Valid; Empty list)`` () = 
    let lst = ListADT<int>(Con(1, Con(2, Con(3, Empty))), 3) :> IPList<int>
    lst.DelLast() |> ignore
    (lst.ToString()) |> should equal "(|1; 2|)"
    let lst = ListADT() :> IPList<int>
    lst.DelLast() |> should equal false

  [<TestCase(2, Result = "(|1; 2|)", TestName = "Remove last elm")>]
  [<TestCase(0, Result = "(|2; 3|)", TestName = "Remove head")>]
  [<TestCase(1, Result = "(|1; 3|)", TestName = "Remove from tail")>]
  [<TestCase(9, Result = "(|1; 2; 3|)", 
                 TestName = "Index out of range")>]
  member this.``DelByIndex`` i = 
    let lst = ListADT<int>(Con(1, Con(2, Con(3, Empty))), 3) 
              :> IPList<int>
    lst.DelByInd i |> ignore
    lst.ToString()



// Task 29 List based on built-in Array
type ListArr<'A when 'A : equality>(arr : 'A []) = 
  class
    let mutable currdata = arr

    new() = ListArr<'A>(Array.empty<'A>)
    new(elm : 'A) = ListArr<'A>([|elm|])

    interface IPList<'A> with
      member this.Head () = Some currdata.[0]
      member this.Tail () = 
          match currdata.Length with
          | 0 -> failwith "No tail in empty list"
          | 1 -> Some (ListArr<'A>() :> IPList<'A>)
          | _ -> 
            Some (ListArr<'A>(currdata.[1..currdata.Length - 1]) 
                  :> IPList<'A>)
        
      member this.Length () = currdata.Length
      member this.AddForw a = currdata <- Array.append [|a|] currdata
      member this.AddBack a = currdata <- Array.append currdata [|a|]
      member this.AddByInd a ind = 
        if (ind < 0) || (ind > currdata.Length) then 
          false
        elif ind = currdata.Length then 
          currdata <- Array.append currdata [|a|]
          true
        else
          currdata <- 
            Array.append 
              (Array.append currdata.[0..ind - 1] [|a|]) 
                (currdata.[ind..currdata.Length - 1])
          true
      member this.DelFirst () = 
        match currdata.Length with
        | 0 -> false
        | _ -> 
          currdata <- currdata.[1..currdata.Length - 1]; true
      member this.DelLast () =
        match currdata.Length with 
        | 0 -> false
        | _ -> 
          currdata <- currdata.[0..currdata.Length - 2]; true
      member this.DelByInd ind = 
        if currdata.Length = 0 then false
        elif (ind < 0) || (ind > currdata.Length - 1) then 
          false
        else
          currdata <- 
            Array.append currdata.[0..ind - 1] 
              currdata.[ind + 1..currdata.Length - 1]
          true
      member this.tryFind f = Array.tryFind f currdata
      member this.Append tail =
        let rec app (ato : IPList<'A>) (lst : IPList<'A>) = 
          match lst.Length() with
          | 0 -> ()
          | _ -> 
            ato.AddBack (lst.Head().Value)
            app ato (lst.Tail().Value)
        app this tail
      member this.Print () = 
        printf "[|"
        for i in currdata do
          printf " %A;" i
        printf " |]"
    
    override this.ToString () = 
      ((sprintf "%A" currdata).Replace("[", "(")).Replace("]", ")")
  end



[<TestFixture>]
type ``Array list test`` () = 
  [<Test>]
  member this.``AddForward`` () = 
    let lst = ListArr(0) :> IPList<int>
    lst.AddForw 1
    (lst.ToString()) |> should equal "(|1; 0|)"
  
  [<Test>]
  member this.``AddBack`` () = 
    let lst = ListArr(0) :> IPList<int>
    lst.AddBack 1
    (lst.ToString()) |> should equal "(|0; 1|)"
  
  [<TestCase(5, 3, Result = "(|1; 0; 2; 5|)",
                   TestName = "Add to tail")>]
  [<TestCase(8, 0, Result = "(|8; 1; 0; 2|)",
                   TestName = "Add head")>]
  [<TestCase(3, 2, Result = "(|1; 0; 3; 2|)",
                   TestName = "Add to middle")>]
  [<TestCase(3, 21, Result = "(|1; 0; 2|)",
                   TestName = "Index out of range")>]
  member this.``AddByIndex`` a i = 
    let lst = ListArr(0) :> IPList<int>
    lst.AddForw 1
    lst.AddBack 2
    lst.AddByInd a i |> ignore
    lst.ToString()

  [<Test>]
  member this.``DelFirst (Valid; Empty list)`` () = 
    let lst = ListArr<int>([|1; 2; 3|]) :> IPList<int>
    lst.DelFirst() |> ignore
    (lst.ToString()) |> should equal "(|2; 3|)"
    let lst = ListADT() :> IPList<int>
    lst.DelFirst() |> should equal false
    
  [<Test>]
  member this.``DelLast (Valid; Empty list)`` () = 
    let lst = ListArr<int>([|1; 2; 3|]) :> IPList<int>
    lst.DelLast() |> ignore
    (lst.ToString()) |> should equal "(|1; 2|)"
    let lst = ListADT() :> IPList<int>
    lst.DelLast() |> should equal false

  [<TestCase(2, Result = "(|1; 2|)", TestName = "Remove last elm")>]
  [<TestCase(0, Result = "(|2; 3|)", TestName = "Remove head")>]
  [<TestCase(1, Result = "(|1; 3|)", TestName = "Remove from tail")>]
  [<TestCase(9, Result = "(|1; 2; 3|)", 
                 TestName = "Index out of range")>]
  member this.``DelByIndex`` i = 
    let lst = ListArr<int>([|1; 2; 3|]) :> IPList<int>
    lst.DelByInd i |> ignore
    lst.ToString()




[<EntryPoint>]
let main argv = 
  0 