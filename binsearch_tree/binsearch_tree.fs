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
  let tr = Empty
  BstPrint "LCR" tr
  printf "\n"

  let tr = BstAdd 8 tr
  let tr = BstAdd 3 tr
  let tr = BstAdd 10 tr
  let tr = BstAdd 6 tr
  let tr = BstAdd 14 tr
  let tr = BstAdd 1 tr
  let tr = BstAdd 7 tr
  let tr = BstAdd 4 tr

  BstPrint "LCR" tr
  printf "\n"

  let tr = BstDel 3 tr
  let tr = BstDel 14 tr

  BstPrint "LCR" tr
  printf "\n"

  BstPrint "LRC" tr
  printf "\n"

  BstPrint "CLR" tr
  printf "\n"
  0