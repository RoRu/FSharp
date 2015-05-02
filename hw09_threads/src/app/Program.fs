// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

module Threads

open System.Threading

//task 46 Max in array 
// Здесь время выполнения только увеличивается, но я не очень понимаю, почему
(*let rec maxElm (arr: int []) threadNum = 
  let alen = arr.Length
  if threadNum = 1 then
    res := Array.max arr
  elif threadNum = 2 then
    let lThread = new Thread(ThreadStart(fun _ ->
      let thMax = Array.max arr.[0..(alen - 1) / 2]
      Monitor.Enter(res)
      if thMax > !res then res := thMax
      Monitor.Exit(res)
    ))
    let rThread = new Thread(ThreadStart(fun _ ->
      let thMax = Array.max arr.[((alen - 1) / 2 + 1)..(alen - 1)]
      Monitor.Enter(res)
      if thMax > !res then res := thMax
      Monitor.Exit(res)
    ))
    lThread.Start()
    rThread.Start()
    lThread.Join()
    rThread.Join()
  else
    let lThread = new Thread(ThreadStart(fun _ ->
      let thMax = maxElm arr.[0..alen / 2] (threadNum / 2)
      Monitor.Enter(res)
      if thMax > !res then res := thMax
      Monitor.Exit(res)
    ))

    let rThread = new Thread(ThreadStart(fun _ ->
      let thMax = maxElm arr.[(alen / 2 + 1)..(alen - 1)] (threadNum / 2)
      Monitor.Enter(res)
      if thMax > !res then res := thMax
      Monitor.Exit(res)
    ))
    lThread.Start()
    rThread.Start()
    lThread.Join()
    rThread.Join()
  !res
*)

let maxElm (arr: int []) threadNum = 
  let step = arr.Length / threadNum
  let res = ref arr.[0]

  let thrArr = Array.init threadNum (fun n ->
    new Thread(ThreadStart(fun _ ->
      let thRes = Array.max arr.[(n * step)..(n + 1) * step - 1]
      Monitor.Enter(res)
      if thRes > !res then res := thRes
      Monitor.Exit(res)
    ))
  )

  for i in thrArr do
    i.Start()
  for i in thrArr do
    i.Join()

  !res  

let duration s f = 
  let timer = new System.Diagnostics.Stopwatch()
  timer.Start()
  let returnValue = f()
  printfn "Task: %s\t\t\tElapsed Time: %i" s timer.ElapsedMilliseconds
  returnValue


[<EntryPoint>]
let main argv = 
  let rnd = new System.Random(0)
  let a = Array.init 10000 (fun i -> rnd.Next(0, 100000))
  //printfn "%A" a
  for i in [("1", 1); ("2", 2); ("4", 4); ("8", 8)] do
    printfn "%d" (maxElm a (snd i))
    (duration (fst i) (fun () -> maxElm a (snd i))) |> ignore
  0