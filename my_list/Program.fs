// Solution for polymprphic list
// by Vladimir Yumatov
// SPBU, group 171

// Expected execution time: 2.5 h
// Real time: 4 h

// Task 27 General interface
type IPList<'A when 'A : equality> = 
  interface
    abstract Head     : unit         -> Option<'A>
    abstract Tail     : unit         -> IPList<'A>
    abstract Length   : unit         -> int
    abstract AddForw  : 'A           -> unit
    abstract AddBack  : 'A           -> unit
    abstract AddByInd : 'A           -> int  -> unit
    abstract DelFirst : unit         -> unit
    abstract DelLast  : unit         -> unit
    abstract DelByInd : int          -> unit
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
    let mutable currsize = basicsize

    new() = ListADT(Empty, 0)
    new(h : 'A) = ListADT(Con(h, Empty), 1)

    interface IPList<'A> with
      member this.Head () = 
        match currdata with
        | Empty -> None
        | Con(h, _) -> Some h
      member this.Tail () = 
        match currdata with
        | Empty -> failwith "No tail in empty list"
        | Con(h, Empty) -> ListADT<'A>() :> IPList<'A>
        | Con(_, t) -> ListADT<'A>(t, currsize - 1) :> IPList<'A>
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
      member this.DelFirst () = 
        match currdata with
        | Empty -> failwith "Nothing is here"
        | Con(h, t) -> 
          currdata <- t
          currsize <- currsize - 1
      member this.DelLast () = 
        let rec del dt = 
          match dt with
          | Empty -> failwith "Nothing is here"
          | Con(h, Con(_, Empty)) -> Con(h, Empty)
          | Con(h, t) -> Con(h, del t)
        currdata <- del currdata
        currsize <- max 0 (currsize - 1)
      member this.DelByInd ind = 
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
            app ato (lst.Tail())
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



// Task 29 List based on built-in Array
type ListArr<'A when 'A : equality>(arr : 'A []) = 
  class
    let mutable currdata = arr
    //let mutable currsize = currdata.Length
    //let a = Array.toList arr

    //do
      //printfn "%s" (a.ToString())

    new() = ListArr<'A>(Array.empty<'A>)
    new(elm : 'A) = ListArr<'A>([|elm|])

    interface IPList<'A> with
      member this.Head () = Some currdata.[0]
      member this.Tail () = 
        match currdata.Length with
        | 0 -> failwith "No tail in empty list"
        | 1 -> ListArr<'A>() :> IPList<'A>
        | _ -> ListArr<'A>(currdata.[1..currdata.Length - 1]) :> IPList<'A>
      member this.Length () = currdata.Length
      member this.AddForw a = 
        currdata <- Array.append [|a|] currdata
       
      member this.AddBack a = 
        currdata <- Array.append currdata [|a|]
       // currsize <- currsize + 1
      member this.AddByInd a ind = 
        currdata <- 
          Array.append 
            (Array.append currdata.[0..ind - 1] [|a|]) 
              (currdata.[ind..currdata.Length - 1])
       // currsize <- currsize + 1
      member this.DelFirst () = 
        match currdata.Length with
        | 0 -> failwith "List is empty"
        | _ -> 
          currdata <- currdata.[1..currdata.Length - 1]
          //currsize <- currsize - 1
      member this.DelLast () =
        match currdata.Length with 
        | 0 -> failwith "List is empty"
        | _ -> 
          currdata <- currdata.[0..currdata.Length - 2]
          //currsize <- currsize - 1
      member this.DelByInd ind = 
        currdata <- 
          Array.append currdata.[0..ind - 1] 
            currdata.[ind + 1..currdata.Length - 1]
        //currsize <- currsize - 1
      member this.tryFind f = 
        Array.tryFind f currdata
      member this.Append tail =
        let rec app (ato : IPList<'A>) (lst : IPList<'A>) = 
          match lst.Length() with
          | 0 -> ()
          | _ -> 
            ato.AddBack (lst.Head().Value)
            app ato (lst.Tail())
        //currsize <- tail.Length()
        app this tail
      member this.Print () = 
        printf "[|"
        for i in currdata do
          printf " %A;" i
        printf " |]\n"
    
    override this.ToString () = currdata.ToString()
  end


[<EntryPoint>]
let main argv = 
  printfn "TASK 28\nEmpty list:\n %A" (ListADT<int>())
  let mutable t = ListADT<int>(1) :> IPList<int>
  printfn "Our list t with 1:\n %A" t
  t.AddForw 0
  t.AddBack 5
  t.AddByInd 3 2
  let tail = ListADT<int>(Con(7, Con(8, Empty)), 2)
  t.Append tail
  printfn "Our list t with added elms:\n %A" t
  t.DelFirst()
  t.DelByInd 2
  printfn "Our list t with some deleted elms:\n %A\nIts lenght is %d"
    t (t.Length())
  printfn "Also, find function is working too:\n 3 is in list: %A\n 2 is not: %A" 
    (t.tryFind (fun k -> k = 3)) (t.tryFind (fun k -> k = 2))
  

  // с выводом всё очень плохо, пришлось делать костыль в виде метода Print
  printfn "\nTASK 29\nEmpty list:\n %A (Я не знаю, что это)" 
    (ListArr<int>()) 
  let t1 = ListArr<int>(1) :> IPList<int>
  printf "Our list t1 with 1:\n " 
  t1.Print()
  t1.AddForw 0
  t1.AddBack 5
  t1.AddByInd 3 2
  let tail = ListArr<int>([|7;8;9|]) :> IPList<int>
  t1.Append tail
  printf "Our list t1 with added elms:\n AddForw 0;  AddBack 5;\n Add 3 to ind 2;  Append (|7;8;9|)\n  " 
  t1.Print()
  t1.DelFirst()
  t1.DelByInd 2
  t1.DelLast()
  printf "Our list t1 with some deleted elms:\n DelFirst;  DelByIndex 2;  DelLast\n  "
  t1.Print() 
  printfn "Its lenght is %d" (t1.Length())
  printfn "Also, find function is working too:\n 8 is in list: %A\n 2 is not: %A" 
    (t1.tryFind (fun k -> k = 8)) (t1.tryFind (fun k -> k = 2))
  0 
