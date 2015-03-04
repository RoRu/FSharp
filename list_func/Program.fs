// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let ins l x = 
  x::l

[<EntryPoint>]
let main argv = 
  let ls = [1;2;3;4]
  let n = []
  let n = List.fold ins n ls
  printfn "%A" n
  0 

