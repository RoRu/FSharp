// Task 14
// Solution for polymorphic binary search tree
// by Vladimir Yumatov
// SPBSU 171 gr.

// Expected execution time: 2.5 h
// Real time: ~3.5 h


// Type for Binary search tree
type BsTree<'A> = 
  | Empty 
  | Item of 'A * BsTree<'A> * BsTree<'A> 

// Add element x to tree t
let rec BstAdd x t =
  match x, t with
  | x, Empty -> Item(x, Empty, Empty)
  | x, Item(y, lft, rgt) ->
    match (compare x y) with
    | c when c > 0  -> Item(y, lft, BstAdd x rgt)
    | c when c < 0 -> Item(y, BstAdd x lft, rgt)
    | _ -> t

// Delete element x from tree t
let rec BstDel x t = 
  match x, t with
  | _, Empty -> Empty
  | _, Item(y, lft, rgt) ->
    match (compare x y) with
    | c when c > 0  -> Item(y, lft, BstDel x rgt)
    | c when c < 0 -> Item(y, BstDel x lft, rgt)
    | _ -> 
      match lft, rgt with
      | Empty, _ -> rgt
      | _, Empty -> lft
      | _, Item(y', Empty, rgt') -> Item(y', lft, rgt')
      | _, Item(y', lft', rgt') ->
        let rec ReplaceSearch t = 
          match t with
          | Empty -> y' 
          | Item(n, Empty, _) -> n
          | Item(_, l, _) -> ReplaceSearch l
        
        let rep = ReplaceSearch lft'
        Item(rep, lft, BstDel rep (Item(y', lft', rgt')))

// Print tree t
let rec BstPrint opt t = 
  match t with
  | Empty -> ()
  | Item(x, lft, rgt) ->
    match opt with
    | "LCR" -> 
      BstPrint opt lft
      printf "%A " x
      BstPrint opt rgt
    | "LRC" ->
      BstPrint opt lft
      BstPrint opt rgt
      printf "%A " x
    | "CLR" -> 
      printf "%A " x
      BstPrint opt lft
      BstPrint opt rgt
    | _ -> printf "%s " "Unknown command\n"



// Task 15 Map for tree
let rec TreeMap f tr = 
  match tr with
  | Empty -> Empty
  | Item(x, lft, rgt) -> Item(f x, TreeMap f lft, TreeMap f rgt)


// Task 16 Fold for tree
let rec TreeFold f res tr = 
  match tr with
  | Empty -> res
  | Item(x, lft, rgt) -> TreeFold f (TreeFold f (f res x) lft) rgt
    //f (f (TreeFold f res lft) (TreeFold f res rgt)) x


// Task 17 Sum of elements in integer tree
let TreeSum tr = TreeFold (fun a b -> a + b) 0 tr 


// Task 18 Find min value in tree
let TreeMin tr = 
  let rec LessElm a b = // Returns the least element out of a, b
    match a with
    | None -> Some b
    | Some a -> Some (min a b)
  TreeFold LessElm None tr


// Task 19 Copy tree
let TreeCopy tr = TreeFold (fun res t -> BstAdd t res) Empty tr


[<EntryPoint>]
let main argv =
  let mutable ts = Empty
  BstPrint "LCR" ts
  printf "\n"

  ts <- BstAdd "x" ts
  ts <- BstAdd "z" ts
  ts <- BstAdd "y" ts
  ts <- BstAdd "d" ts
  ts <- BstAdd "l" ts

  printfn "Task 14 - Tree with stings:"
  BstPrint "LCR" ts
  printf "\n"

  ts <- BstDel "x" ts
  ts <- BstDel "l" ts

  printfn "\nSame tsee with two deleted elements:"
  BstPrint "LCR" ts
  printf "\n"

  let mutable tr = Empty

  tr <- BstAdd 4 tr
  tr <- BstAdd 2 tr
  tr <- BstAdd 1 tr
  tr <- BstAdd 3 tr
  tr <- BstAdd 9 tr
  tr <- BstAdd 10 tr

  printfn "\nTest tree:"
  BstPrint "LCR" tr


  let n = TreeMap (fun z -> z * 2) tr
  printfn "\n\nTask 15 (Map) - Doubled elements of tree:"
  BstPrint "LCR" n;

  let n = TreeFold (fun a b -> if (b % 2 = 0) then a + b else a) 0 tr
  printfn "\n\nTask 16 (Fold) - Sum of even elements:\n2+4+10 = %d" n

  let n = TreeSum tr
  printfn "\nTask 17 - Sum of elements:\n4+2+1+3+9+10 = %d\n" n

  let n = TreeMin tr
  printfn "Task 18 - The least element is %A\n" n

  let n = TreeCopy tr
  printfn "Task 19 - Copying of tree:"
  BstPrint "LCR" n
  printfn "\n"
  0
