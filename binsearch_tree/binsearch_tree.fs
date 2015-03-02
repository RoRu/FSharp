// Solution for binary search tree
// by Vladimir Yumatov
// SPBSU 171 gr.

// Предполагаемое время выполнения: 2 часа
// Реальное время выполнения: 2.5 часа

open System

type BsTree = 
  | Empty 
  | Item of int * BsTree * BsTree


let rec BstAdd x t =
  match x, t with
  | x, Empty -> Item(x, Empty, Empty)
  | x, Item(y, lft, rgt) ->
    match (compare x y) with
    | 1  -> Item(y, lft, BstAdd x rgt)
    | -1 -> Item(y, BstAdd x lft, rgt)
    | _ -> Item(y, lft, rgt)

let rec BstDel x t = 
  match x, t with
  | _, Empty -> Empty
  | x, Item(y, lft, rgt) ->
    match (compare x y) with
    | 1  -> Item(y, lft, BstDel x rgt)
    | -1 -> Item(y, BstDel x lft, rgt)
    | _ -> 
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
  let mutable tr = Empty
  BstPrint "LCR" tr
  printf "\n"

  tr <- BstAdd 8 tr
  tr <- BstAdd 3 tr
  tr <- BstAdd 10 tr
  tr <- BstAdd 6 tr
  tr <- BstAdd 14 tr
  tr <- BstAdd 1 tr
  tr <- BstAdd 7 tr
  tr <- BstAdd 4 tr

  BstPrint "LCR" tr
  printf "\n"

  tr <- BstDel 3 tr
  tr <- BstDel 14 tr

  BstPrint "LCR" tr
  printf "\n"

  BstPrint "LRC" tr
  printf "\n"

  BstPrint "CLR" tr
  printf "\n"
  0