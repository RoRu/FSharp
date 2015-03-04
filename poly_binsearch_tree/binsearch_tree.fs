// Solution for polymorphic binary search tree
// by Vladimir Yumatov
// SPBSU 171 gr.

type BsTree<'A> = 
  | Empty 
  | Item of 'A * BsTree<'A> * BsTree<'A> 


let rec BstAdd x t =
  match x, t with
  | x, Empty -> Item(x, Empty, Empty)
  | x, Item(y, lft, rgt) ->
    match (compare x y) with
    | c when c > 0  -> Item(y, lft, BstAdd x rgt)
    | c when c < 0 -> Item(y, BstAdd x lft, rgt)
    | _ -> Item(y, lft, rgt)

let rec BstDel x t = 
  match x, t with
  | _, Empty -> Empty
  | _, Item(y, lft, rgt) ->
    match (compare x y) with
    | c when c > 0  -> Item(y, lft, BstDel x rgt)
    | c when c < 0 -> Item(y, BstDel x lft, rgt)
    | _ -> 
      match lft, rgt with
      | Empty, Empty -> Empty
      | Empty, rgt -> rgt
      | lft, Empty -> lft
      | lft, Item(y', Empty, rgt') -> Item(y', lft, rgt')
      | lft, Item(y', lft', rgt') ->
        let rec ReplaceSearch t = 
          match t with
          | Empty -> y' 
          | Item(n, Empty, _) -> n
          | Item(_, l, _) -> ReplaceSearch l
        
        let rep = ReplaceSearch lft'
        Item(rep, lft, BstDel rep (Item(y', lft', rgt')))

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


[<EntryPoint>]
let main argv =
  let mutable tr = Empty
  BstPrint "LCR" tr
  printf "\n"

  tr <- BstAdd "x" tr
  tr <- BstAdd "z" tr
  tr <- BstAdd "y" tr
  //tr <- BstAdd 'a' tr
  tr <- BstAdd "d" tr
  //tr <- BstAdd 'g' tr
  //tr <- BstAdd 'l' tr
  //tr <- BstAdd 'x' tr
  //tr <- BstAdd 'e' tr

  BstPrint "LCR" tr
  printf "\n"

  tr <- BstDel "x" tr
 // tr <- BstDel 'e' tr

  BstPrint "LCR" tr
  printf "\n"

  BstPrint "LRC" tr
  printf "\n"

  BstPrint "CLR" tr
  printf "\n"
  0