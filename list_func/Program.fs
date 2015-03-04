// Solution for functions of List through List.fold
// by Yumatov Vladimir
// SPBSU 171 gr.

let Reverse ls = 
  let ins l x = 
    x :: l
  let n = []
  List.fold ins n ls

let Filter f ls =
  let foo n x = 
    match f(x) with 
    | true -> x::n
    | false -> n
  let l = []
  Reverse (List.fold foo l ls)
  
let Map f ls = 
  let foo n x =
    f(x)::n
  let l = []
  Reverse (List.fold foo l ls)

[<EntryPoint>]
let main argv = 
  let ls = [1; 2; 3; 4; 5; 6]

  let n = Reverse ls
  printfn "Reversed list %A is %A" ls n

  let fil z = 
    z % 2 = 0
  let n = Filter fil ls
  printfn "Even elements of %A are %A" ls n

  let Dbl z = 
    z * 2
  let n = Map Dbl ls
  printfn "Doubled elements of %A are %A" ls n
  0 
