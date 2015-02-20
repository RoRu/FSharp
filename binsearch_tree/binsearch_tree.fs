// Solution for binary search tree
// by Vladimir Yumatov
// SPBSU 171 gr.

open System

type BsTree = 
  | Empty 
  | Item of int * BsTree * BsTree


let rec BstAdd x t =
  match x, t with
  | x, Empty -> Item(x, Empty, Empty)
  | x, Item(y, lft, rgt) ->
    if x > y then Item(y, lft, BstAdd x rgt)
    else if x < y then Item(y, BstAdd x lft, rgt)
    else Item(y, lft, rgt)

let rec BstDel x t = 
  match x, t with
  | _, Empty -> Empty
  | x, Item(y, lft, rgt) ->
    if x > y then Item(y, lft, BstDel x rgt)
    else if x < y then Item(y, BstDel x lft, rgt)
    else
      match lft, rgt with
      | Empty, Empty -> Empty
      | Empty, rgt -> rgt
      | lft, Empty -> lft
      | lft, Item(y', Empty, rgt') -> Item(y', lft, rgt')
      | lft, Item(y1, lft1, rgt1) ->
        let rec ReplaceSearch t = 
          match t with
          | Empty -> 0
          | Item(n, Empty, _) -> n
          | Item(n, l, _) -> ReplaceSearch l
       
        let rep = ReplaceSearch lft1
        Item(rep, lft, BstDel rep (Item(y1, lft1, rgt1)))

let rec BstPrint opt t = 
  match t with
  | Empty -> ()
  | Item(x, lft, rgt) ->
    match opt with
    | "LCR" -> 
      BstPrint opt lft
      printf "%d " x
      BstPrint opt rgt
    | "LRC" ->
      BstPrint opt lft
      BstPrint opt rgt
      printf "%d " x
    | "CLR" -> 
      printf "%d " x
      BstPrint opt lft
      BstPrint opt rgt
    | _ -> printf "%s " "Unknown command\n"


[<EntryPoint>]
let main argv =
  Console.Write("How many nodes do you want: ")
  let n = Console.ReadLine() |> int

  let rec GetTree nodes t =
    match nodes with
    | 0 -> t
    | _ ->
      let k = Console.ReadLine() |> int
      let t' = BstAdd k t
      GetTree (nodes-1) t'
  
  let tr = Empty
  Console.Write("Enter your numbers: ")
  let tr = GetTree n tr

  Console.Write("Enter LCR, LRC, CLR: ")
  let printOption = Console.ReadLine()
  BstPrint printOption tr
  0