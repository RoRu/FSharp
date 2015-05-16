// Tasks 46
// by Vladimir Yumatov
// SPBU, group 171

// Expected time: 1.5 h
// Real time: 2.25 h 

module Threads

open System.Threading

//task 46 Max in array 
let maxElm (arr: int []) tNum = 
  let mutable threadNum = 0
  if tNum > arr.Length then threadNum <- arr.Length
  else threadNum <- tNum
  let step = arr.Length / threadNum
  let res = ref arr.[0]

  let thrArr = Array.init (threadNum - 1) (fun n ->
    new Thread(ThreadStart(fun _ ->
      let thRes = Array.max arr.[(n * step)..(n + 1) * step - 1]
      Monitor.Enter(res)
      if thRes > !res then res := thRes
      Monitor.Exit(res)
    ))
  )
  let tn = threadNum
  let thrLast = new Thread(ThreadStart(fun _ ->
    let thRes = Array.max arr.[((tn - 1) * step)..arr.Length - 1]
    Monitor.Enter(res)
    if thRes > !res then res := thRes
    Monitor.Exit(res)
  ))

  for i in thrArr do
    i.Start()
  thrLast.Start()
  for i in thrArr do
    i.Join()
  thrLast.Join()

  !res  

let duration s f = 
  let timer = new System.Diagnostics.Stopwatch()
  timer.Start()
  let returnValue = f()
  printfn "Task: %s\t\t\tElapsed Time: %i" s timer.ElapsedMilliseconds
  returnValue


[<EntryPoint>]
let main argv = 
  let rnd = new System.Random(1)
  let a = Array.init 10 (fun i -> rnd.Next(0, 1000))
  //printfn "%A" a
  for i in [("1", 1); ("2", 2); ("4", 4); ("8", 8); ("16", 16)] do
    printfn "%d" (maxElm a (snd i))
    (duration (fst i) (fun () -> maxElm a (snd i))) |> ignore
  0