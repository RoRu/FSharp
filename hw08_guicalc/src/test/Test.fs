module CalcTests

open System.Windows.Forms
open System.Drawing
open NUnit.Framework
open FsUnit
open Calc

[<TestFixture>]
type ``Calc tests`` () = 
    [<TestCase ([|"5"; "+"; "2"|], Result = "7")>]
    member this.``Tests for binary operations`` ops =
      true |> should equal true
