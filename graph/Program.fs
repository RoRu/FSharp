// Tasks 20-25
// Solution for graphs
// by Vladimir Yumatov
// SPBU, group 171

// Expected execution time: 4 h.
// Real time: ~5.5 h


// Task 20 General graph interface
type IGraph<'A> =
  interface
    // returns number of nodes
    abstract Size     : unit -> int
    // returns value by index of node
    abstract Val      : int  -> 'A
    // returns true if there is edge between two nodes
    abstract IsEdge   : int  -> int -> bool // 
    // return list of nodes that are available 
    // from this one through only one edge
    abstract OutNodes : int  -> int list
    // prints the graph
    abstract Print    : unit -> unit
  end

// Task 21 Graph with adjacency matrix
type AdjMxGraph<'A> (nodes : 'A list, e : (int * int) list) = 
  class
    let size = nodes.Length
    let edges = Array2D.create size size false

    do 
      for (n1, n2) in e do
        edges.[n1, n2] <- true

    interface IGraph<'A> with
      member this.Size () = size
      member this.Val ind = nodes.[ind]
      member this.IsEdge n1 n2 = edges.[n1, n2]
      member this.OutNodes nd = 
        let mutable res : int list = []
        for i in 0 .. size - 1  do
          if edges.[nd, i] then res <- res @ [i]
        res
      member this.Print () = 
        printfn "Nodes: %A" nodes
        printfn "Edges: %A" edges
  end


// Tack 22 Graph with adjacency list
type AdjListGraph<'A> (nodes : 'A list, e : (int * int) list) = 
  class
    let size = nodes.Length
    let edges = Array.create size (Array.create 0 0)

    do 
      for (n1, n2) in e do
        Array.set edges n1 (Array.append edges.[n1] [|n2|])

    interface IGraph<'A> with
      member this.Size () = size
      member this.Val ind = nodes.[ind]
      member this.IsEdge n1 n2 = 
        Array.tryFind (fun k -> k = n2) edges.[n1] = Some n2
      member this.OutNodes nd = Array.toList edges.[nd]
      member this.Print () = 
        printfn "Nodes: %A" nodes
        printfn "Edges: %A" edges
  end


// Task 23 Returns all the nodes
// to which there's a way from node 'n'
let NodesFrom (gr : IGraph<'A>) n = 
  let s = gr.Size()
  let visited = Array.create s false
  let rec dfs n' = 
    if not(visited.[n']) then
      visited.[n'] <- true
      for i in (gr.OutNodes n') do
        dfs i
    else ()
  dfs n
  (List.filter (Array.get visited) [0..s-1]).Tail


// Task 24 Returns all the nodes 
// from which there's a way to node 'n'
let NodesTo (gr : IGraph<'A>) n = 
  let s = gr.Size()
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
  List.filter find [0..s-1]


// Task 25 Interface for polymorphic marked graph 
type IPolyMarkedGraph<'A, 'B> = 
  interface
    inherit IGraph<'A>

    abstract isMarked : int -> bool
    abstract getMark  : int -> 'B
    abstract giveMark : int -> 'B -> unit
  end


[<EntryPoint>]
let main argv = 
    let c = AdjMxGraph(['A'; 'B'; 'C'; 'D'], [(0, 1); (1, 2); (3, 2)])
    printfn "TASK 21"
    (c :> IGraph<char>).Print()
    printfn "\nThere's edge 2-1: %A" ((c :> IGraph<char>).IsEdge 2 1)
    printfn "Size of graph: %d" ((c :> IGraph<char>).Size())
    printfn "Nodes available from node 0: %A" 
      ((c :> IGraph<char>).OutNodes 0)
    printfn "Value of the first node: %A\n" ((c :> IGraph<char>).Val 0)
    printfn "\nTASK 23\nAll the nodes available from node 0:\n %A" 
      (NodesFrom c 0)
    printfn "\nTASK 24\nAll the nodes from which there's way to node 2:\n %A" 
      (NodesTo c 2)

    let c1 = AdjListGraph([1; 2; 3], [(0, 1); (1, 2); (0, 2)])
    printfn "\nTASK 22"
    (c1 :> IGraph<int>).Print()
    printfn "\nThere's edge 1-2: %A" ((c1 :> IGraph<int>).IsEdge 1 2)
    printfn "Size of graph: %d" ((c1 :> IGraph<int>).Size())
    printfn "Nodes available from node 0: %A" 
      ((c1 :> IGraph<int>).OutNodes 0)
    printfn "Value of the first node: %A" ((c1 :> IGraph<int>).Val 0)
    0 
    