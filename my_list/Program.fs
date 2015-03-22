// Solution for polymprphic list
// by Vladimir Yumatov
// SPBU, group 171

// Expected execution time: 2.5 h
// Real time: 4 h

// Task 27 General interface
type IPList<'A when 'A : equality> = 
  interface
    abstract Head     : unit         -> Option<'A>
    //abstract Tail     : unit         -> IPList<'A>
    abstract Length   : unit         -> int
    abstract AddForw  : 'A           -> IPList<'A>
    abstract AddBack  : 'A           -> IPList<'A>
    abstract AddByInd : 'A           -> int       -> IPList<'A>
    abstract DelFirst : unit         -> IPList<'A>
    abstract DelLast  : unit         -> IPList<'A>
    abstract DelByInd : int          -> IPList<'A>
    abstract tryFind  : ('A -> bool) -> Option<'A>
    abstract Append   : IPList<'A>   -> IPList<'A>
    abstract Print    : unit         -> unit
  end


type Item<'A> = 
  | Empty
  | It of 'A * Item<'A>


// Task 28 List based on ADT
type ListADT<'A when 'A : equality> (hd : Item<'A>, size : int) = 
  class
    new() = ListADT(Empty, 0)
    new(h : 'A) = ListADT(It(h, Empty), 1)

    interface IPList<'A> with
      member this.Head () = 
        match hd with
        | Empty -> None
        | It(h, _) -> Some h
      member this.Length () = size
      member this.AddForw a = 
        ListADT(It(a, hd), size + 1) :> IPList<'A>
      member this.AddBack a = 
        let rec foo n m = 
          match n with
          | Empty -> It(m, Empty)
          | It(n', m') -> It(n', foo m' m)
        ListADT(foo hd a, size + 1) :> IPList<'A>
      member this.AddByInd a ind = 
        let rec foo item a' i = 
          match item with 
          | Empty -> It(a', Empty)
          | It(h, t) -> 
            match (compare i ind) with
            | c when c < 0 -> It(h, foo t a' (i + 1))
            | _ -> It(a', item)
        ListADT(foo hd a 0, size + 1) :> IPList<'A>
      member this.DelFirst () = 
        match hd with
        | Empty -> failwith "There's nothing already"
        | It(h, t) -> ListADT(t, size - 1) :> IPList<'A>
      member this.DelLast () = 
        let rec foo nd = 
          match nd with
          | Empty -> failwith "There's nothing already"
          | It(h, It(_, Empty)) -> It(h, Empty)
          | It(h, t) -> It(h, foo t)
        ListADT(foo hd, max 0 (size - 1)) :> IPList<'A>
      member this.DelByInd ind = 
        if ind = 0 then 
          (this :> IPList<'A>).DelFirst()
        elif ind = size - 1 then 
          (this :> IPList<'A>).DelLast()
        else
          let rec foo nd i = 
            match nd with
            | It(h, It(h', t')) ->
              if (i = ind - 1) then 
                It(h, t')
              else 
                It(h, foo (It(h', t')) (i + 1))
            | _ -> Empty
          ListADT(foo hd 0, size - 1) :> IPList<'A>
      member this.tryFind f = 
        let rec foo nd = 
          match nd with
          | Empty -> None
          | It(h, t) ->
            if f h then Some h
            else foo t
        foo hd
      member this.Append tail = 
        let rec itemFrom (l : IPList<'A>) = 
          match l.Length() with
          | 0 -> Empty
          | _ -> It(l.Head().Value, itemFrom (l.DelFirst()))
        let rec foo nd1 nd2 = 
          match nd1 with
          | Empty -> nd2
          | It(h, t) -> It(h, foo t nd2)
        ListADT(foo hd (itemFrom tail), size + tail.Length()) 
        :> IPList<'A>

      member this.Print () = ()

    override this.ToString () =  
      let rec foo nd = 
        match nd with
        | Empty -> "|)"
        | It(h, Empty) -> h.ToString() + "|)"
        | It(h, t) -> h.ToString() + "; " + foo t
      "(|" + foo hd
  end



// Task 29 List based on built-in Array
type ListArr<'A when 'A : equality>(arr : 'A []) = 
  class
    let size = arr.Length
    //let a = Array.toList arr

    //do
      //printfn "%s" (a.ToString())

    new() = ListArr<'A>(Array.empty<'A>)
    new(elm : 'A) = ListArr<'A>([|elm|])

    interface IPList<'A> with
      member this.Head () = Some arr.[0]
      member this.Length () = size
      member this.AddForw a = 
        ListArr<'A>(Array.append [|a|] arr) :> IPList<'A>
      member this.AddBack a = 
        ListArr<'A>(Array.append arr [|a|]) :> IPList<'A>
      member this.AddByInd a ind = 
        let elm = 
          Array.append 
            (Array.append arr.[0..ind - 1] [|a|]) (arr.[ind..size - 1])
        ListArr<'A>(elm) :> IPList<'A>
      member this.DelFirst () = 
        match size with
        | 0 -> failwith "List is empty"
        | _ -> ListArr<'A>(arr.[1..size - 1]) :> IPList<'A>
      member this.DelLast () =
        match size with 
        | 0 -> failwith "List is empty"
        | _ -> ListArr<'A>(arr.[0..size - 2]) :> IPList<'A>
      member this.DelByInd ind = 
        ListArr<'A>(
          Array.append arr.[0..ind - 1] arr.[ind + 1..size - 1])
            :> IPList<'A>
      member this.tryFind f = 
        Array.tryFind f arr
      member this.Append tail = 
        let rec arrFrom (l : IPList<'A>) = 
          match l.Length() with
          | 0 -> [||]
          | _ -> Array.append [|l.Head().Value|] (arrFrom (l.DelFirst()))
        ListArr<'A>(Array.append arr (arrFrom tail)) :> IPList<'A>
      member this.Print () = 
        printf "[|"
        for i in arr do
          printf " %A;" i
        printf " |]\n"
    
    override this.ToString () = arr.ToString()
  end


[<EntryPoint>]
let main argv = 
  printfn "TASK 28\nEmpty list:\n %A" (ListADT<int>())
  let mutable t = ListADT<int>(1) :> IPList<int>
  printfn "Our list t with 1:\n %A" t
  t <- t.AddForw 0
  t <- t.AddBack 5
  t <- t.AddByInd 3 2
  let tail = ListADT<int>(It(7, It(8, Empty)), 2)
  t <- t.Append tail
  printfn "Our list t with added elms:\n %A" t
  t <- t.DelFirst()
  t <- t.DelByInd 2
  printfn "Our list t with some deleted elms:\n %A\nIts lenght is %d"
    t (t.Length())
  printfn "Also, find function is working too:\n 3 is in list: %A\n 2 is not: %A" 
    (t.tryFind (fun k -> k = 3)) (t.tryFind (fun k -> k = 2))
  

  // с выводом всё очень плохо, пришлось делать костыль в виде метода Print
  printfn "\nTASK 29\nEmpty list:\n %A (Я не знаю, что это)" 
    (ListArr<int>()) 
  let mutable t1 = ListArr<int>(1) :> IPList<int>
  printfn "Our list t1 with 1: " 
  t1.Print()
  t1 <- t1.AddForw 0
  t1 <- t1.AddBack 5
  t1 <- t1.AddByInd 3 2
  let tail = ListArr<int>([|7;8;9|])
  t1 <- t1.Append tail
  printfn "Our list t1 with added elms: " 
  t1.Print()
  t1 <- t1.DelFirst()
  t1 <- t1.DelByInd 2
  printfn "Our list t1 with some deleted elms: "
  t1.Print() 
  printfn "Its lenght is %d" (t1.Length())
  printfn "Also, find function is working too:\n 8 is in list: %A\n 2 is not: %A" 
    (t1.tryFind (fun k -> k = 8)) (t1.tryFind (fun k -> k = 2))
  0 
