// Task 31
// Solution for tests on tasks 23-24
// by Vladimir Yumatov
// SPBU, group 171

// Expected execution time: 2.5 h.
// Real time: 2.5 h

open NUnit.Framework
open FsUnit

// General graph interface
type IGraph<'A> =
  interface
    abstract Size     : unit -> int
    abstract Val      : int  -> 'A
    abstract IsEdge   : int  -> int -> bool 
    abstract OutNodes : int  -> int list
    abstract InNodes  : int  -> int list
    abstract Print    : unit -> unit
  end

// Graph with adjacency matrix
type AdjMxGraph<'A> (nodes : 'A list, e : (int * int) list) = 
  class
    let size = nodes.Length
    let edges = Array2D.create size size false

    do 
      try
        for (n1, n2) in e do
          edges.[n1, n2] <- true
      with
      | _ -> printfn "Ivalid data"

    interface IGraph<'A> with
      member this.Size () = size
      member this.Val ind = nodes.[ind]
      member this.IsEdge n1 n2 = 
        if n1 = n2 then true else edges.[n1, n2]
      member this.OutNodes nd = 
        let mutable res : int list = []
        for i in 0 .. size - 1  do
          if edges.[nd, i] then res <- i::res
        List.rev res
      member this.InNodes nd = 
        let mutable res : int list = []
        for i in 0 .. size - 1  do
          if edges.[i, nd] then res <- i::res
        List.rev res
      member this.Print () = 
        printfn "Nodes: %A" nodes
        printfn "Edges:\n%A" edges
  end

[<Test>]
[<ExpectedException>]
let ``Invalid data exception (Matrix)`` () = 
  (AdjMxGraph([], [(1, 2)])) |> should throw typeof<System.Exception>

// Graph with adjacency list
type AdjListGraph<'A> (nodes : 'A list, e : (int * int) list) = 
  class
    let size = nodes.Length
    let edges = Array.create size List.empty

    do 
      try
        for (n1, n2) in e do
          Array.set edges n1 (n2::edges.[n1])
        for i in 0..size-1 do
          Array.set edges i (List.rev edges.[i])
      with
      | _ -> printfn "Invalid data"
      

    interface IGraph<'A> with
      member this.Size () = size
      member this.Val ind = nodes.[ind]
      member this.IsEdge n1 n2 = 
        List.tryFind ((=) n2) edges.[n1] = Some n2
      member this.OutNodes nd = 
          edges.[nd]
      member this.InNodes nd = 
        let mutable res : int list = []
        for i in 0 .. size - 1  do
          if (List.tryFind ((=) nd) edges.[i]) = Some nd then 
            res <- i::res
        List.rev res
      member this.Print () = 
        printfn "Nodes: %A" nodes
        printfn "Edges: %A" edges
  end


[<Test>]
[<ExpectedException>]
let ``Invalid data exception (List)`` () = 
  (AdjListGraph([], [(1, 2)])) |> should throw typeof<System.Exception>


// Task 23 Returns all the nodes
// to which there's a way from node 'n'
let AvailableFromThis (gr : IGraph<'A>) n = 
  let s = gr.Size()
  if (n < 0) || (n > s - 1) then printfn "Index out of range"; [-1] 
  else
    let visited = Array.create s false
    let rec dfs n' = 
      if not(visited.[n']) then
        visited.[n'] <- true
        for i in (gr.OutNodes n') do
          dfs i
      else ()
    dfs n
    List.filter ((<>) n) (List.filter (Array.get visited) [0..s-1])


// Task 24 Returns all the nodes 
// from which there's a way to node 'n'
let HaveAccessToThis (gr : IGraph<'A>) n = 
  let s = gr.Size()
  if (n < 0) || (n > s - 1) then printfn "Index out of range"; [-1]
  else
    let find a =   
      let visited = Array.create s false
      let rec dfs nd =
        if nd = n then
          true
        elif not(visited.[nd]) then
          visited.[nd] <- true
          List.fold (fun res nd -> res || (dfs nd)) false (gr.OutNodes nd)
        else false
      dfs a 
    List.filter ((<>) n) (List.filter find [0..s-1])


// 1<-- 5 --> 2  
// ^ \  |
// |  v v
// 4 -- 6     0
//
// 7<--9 -->8
//     |  /
//     v /
//     3 


[<TestFixture>]
type ``Graph with adj matrix``() = 
  let g = AdjMxGraph<int>([0..9], [(1, 6); (3, 8); (4, 1); (4, 6); 
                                   (5, 1); (5, 2); (5, 6); (6, 4); 
                                   (8, 3); (9, 3); (9, 7); (9, 8)]) 
                                   :> IGraph<int>


  [<TestCase(1, Result = [|4; 6|])>] 
  [<TestCase(0, Result = [||])>]
  [<TestCase(9, Result = [|3; 7; 8|])>]
  [<TestCase(5, Result = [|1; 2; 4; 6|])>]
  [<TestCase(20, Result = [|-1|], TestName = "Index out of range")>]
  member this.``Different cases for AvailableFromThis`` node = 
    List.toArray (AvailableFromThis g node)

  [<TestCase(1, Result = [|4; 5; 6|])>] 
  [<TestCase(0, Result = [||])>]
  [<TestCase(8, Result = [|3; 9|])>]
  [<TestCase(2, Result = [|5|])>]
  [<TestCase(20, Result = [|-1|], TestName = "Index out of range'")>]
  member this.``Different cases for HaveAccessToThis`` node = 
    List.toArray (HaveAccessToThis g node)
   

// Same graph
// 1<-- 5 --> 2  
// ^ \  |
// |  v v
// 4 -- 6     0
//
// 7<--9 -->8
//     |  /
//     v /
//     3 

[<TestFixture>]
type ``Graph with adj list``() = 
  let g = AdjListGraph<int>([0..9], [(1, 6); (3, 8); (4, 1); (4, 6); 
                                   (5, 1); (5, 2); (5, 6); (6, 4); 
                                   (8, 3); (9, 3); (9, 7); (9, 8)]) 
                                   :> IGraph<int>


  [<TestCase(3, Result = [|8|])>] 
  [<TestCase(0, Result = [||])>]
  [<TestCase(9, Result = [|3; 7; 8|])>]
  [<TestCase(5, Result = [|1; 2; 4; 6|])>]
  [<TestCase(20, Result = [|-1|], TestName = "Index out of range")>]
  member this.``Different cases for AvailableFromThis`` node = 
    List.toArray (AvailableFromThis g node)

  [<TestCase(1, Result = [|4; 5; 6|])>] 
  [<TestCase(0, Result = [||])>]
  [<TestCase(8, Result = [|3; 9|])>]
  [<TestCase(2, Result = [|5|])>]
  [<TestCase(20, Result = [|-1|], TestName = "Index out of range'")>]
  member this.``Different cases for HaveAccessToThis`` node = 
    List.toArray (HaveAccessToThis g node)



[<EntryPoint>]
let main argv =
  let g1 = AdjMxGraph<int>([0..9], [(1, 6); (3, 8); (4, 1); (4, 6); 
                                   (5, 1); (5, 2); (5, 6); (6, 4); 
                                   (8, 3); (9, 3); (9, 7); (9, 8)]) 
                                   :> IGraph<int>
  let l = AvailableFromThis g1 17
  printfn "%A" l
  0 