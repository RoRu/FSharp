module Test

open NUnit.Framework
open Threads

[<Test>]
let ``Max of 10^4 elements`` () =
  let a = [|1..10000|]
  for n in [1; 2; 4; 8] do
    Assert.AreEqual(maxElm a n, 10000)

[<Test>]
let ``Max of 10^8 elements`` () =
  let a = [|1..100000000|]
  for n in [1; 2; 4; 8] do
    Assert.AreEqual(maxElm a n, 100000000)

[<Test>]
let ``Max in array with a lot of random elms 4`` () = 
  let rnd = new System.Random(1)
  let a = Array.init 10 (fun i -> rnd.Next(0, 1000))
  for n in [1; 2; 4; 8; 16] do
    Assert.AreEqual(maxElm a n, 943)

[<Test>]
let ``Max in array with a lot of random elms`` () = 
  let rnd = new System.Random(1)
  let a = Array.init 10000000 (fun i -> rnd.Next(0, 10000000))
  for n in [1; 2; 4; 8; 16] do
    Assert.AreEqual(maxElm a n, 9999999)

[<Test>]
let ``Max in array with a lot of random elms 2`` () = 
  let rnd = new System.Random(2)
  let a = Array.init 10000000 (fun i -> rnd.Next(0, 100000000))
  for n in [1; 2; 4; 8; 16] do
    Assert.AreEqual(maxElm a n, 99999990)

[<Test>]
let ``Max in array with a lot of random elms 3`` () = 
  let rnd = new System.Random(2)
  let a = Array.init 100000000 (fun i -> rnd.Next(0, 100000000))
  for n in [1; 2; 4; 8; 16] do
    Assert.AreEqual(maxElm a n, 99999999)