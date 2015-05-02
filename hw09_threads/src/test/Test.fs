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
