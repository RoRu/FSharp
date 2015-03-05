// Solution for functions of List via List.fold
// by Yumatov Vladimir
// SPBSU 171 gr.

// Ожидаемое время: 2 часа
// Реальное время: 3.5 часов

// Task 9
// List.iter : ('a -> unit) -> 'a list -> unit

// Task 10 
let Reverse ls =
  List.fold (fun res elm -> elm::res) [] ls


// Task 11 
let Filter f ls =
  List.foldBack (fun elm res -> 
                 if f(elm) then elm::res else res) ls []


// Task 12 
let Map f ls = 
  List.foldBack (fun elm res -> f(elm)::res) ls []


// Task 13 Horner's method
let Horner a cfs = 
  List.fold (fun res elm -> elm + res * a) 0 cfs


[<EntryPoint>]
let main argv = 
  let ls = [1; 2; 3; 4; 5; 6]

  let n = Reverse ls
  printfn "Reversed list %A is %A\n" ls n

  let fil z = 
    z % 2 = 0
  let n = Filter fil ls
  printfn "Filter: Even elements of %A are %A\n" ls n

  let Dbl z = 
    z * 2
  let n = Map Dbl ls
  printfn "Map: Doubled elements of %A are %A\n" ls n


  let cfs = [4; 5; 9; 1] // 4x^3 + 5x^2 + 9x + 1
  let x0 = 5
  let n = Horner x0 cfs
  printfn "Horner result with coefficients %A is %d\n" cfs n
  0 
