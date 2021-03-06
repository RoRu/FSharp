﻿// Task 35. Calculator and tests for it
// by Vladimir Yumatov
// SPBU, group 171

// Expected execution time: 3-3.5 h
// Real time: 7 h 


open NUnit.Framework
open FsUnit

type Stack<'A> () = 
  class
    let mutable st : 'A list = []
    
    member this.Length = st.Length
    member this.Push elm = st <- elm::st
    
    member this.Pop () =
      if st.Length = 0 then failwith "Stack is empty"
      else
        let res = st.[0]
        st <- st.Tail
        res

    member this.Top () = 
      if st.Length = 0 then failwith "Stack is empty"
      else st.[0]

    override this.ToString () = sprintf "%A" st
  end


let prior operator =
  match operator with
  | '+' -> 1
  | '-' -> 1
  | '*' -> 2
  | '/' -> 2
  | '%' -> 2
  | '^' -> 3
  | _   -> 0



// translates string to an actual expression in postfix notation 
// and divides it into tokens
let translate (e : string) =
  let exp = "(" + e.Replace(" ", "") + ")"
  //printfn "%A\n" exp
  let ostc = Stack<char>()
  let res = Stack<string>()

  let mutable temp = ""
  for i in 0..exp.Length - 1 do
    let ch = exp.[i]
    //printfn "%A   %A   %A   %A" ch temp stc res
    if (System.Char.IsLetterOrDigit(ch)) then 
      temp <- temp + ch.ToString()
    else
      match ch with
      | '-' when (System.Char.IsLetterOrDigit(exp.[i + 1]) &&
                  exp.[i - 1] = '(') ->
        temp <- "-"
      | c when prior(c) > 0 ->
        if System.Char.IsLetterOrDigit(exp.[i - 1]) then
          res.Push(temp)
          temp <- ""
        while ostc.Length > 0 && 
              prior(ostc.Top()) >= prior(ch) && prior(ch) < 3
          do res.Push(ostc.Pop().ToString())
        ostc.Push(ch)
      | '(' -> ostc.Push(ch)
      | ')' ->
        if temp.Length > 0 then
          res.Push(temp)
          temp <- "" 
        while ostc.Length > 0 && ostc.Top() <> '(' do
          res.Push(ostc.Pop().ToString())
        if ostc.Length > 0 then ostc.Pop() |> ignore
      | _ -> ()
    
  res

// returns result of the expression
let calculate exp (vars: string list) (vals: int list) =
  let opstack = translate(exp)
  printfn "\n%A" opstack

  let rec apply operator =
    let mutable a = 0
    let mutable b = 0
    let mutable temp = opstack.Pop()
    let isOperator (a : string) = 
      a.Length = 1 && prior(a.[0]) > 0

    if isOperator(temp) then a <- apply temp
    elif System.Char.IsLetter(temp.[0]) then 
      a <- vals.[List.findIndex ((=) temp) vars]
    elif temp.Length > 1 && temp.[0] = '-' &&
         System.Char.IsLetter(temp.[1]) then
      a <- -vals.[List.findIndex ((=) temp.[1..temp.Length - 1]) vars]
    else a <- int temp
    temp <- opstack.Pop()
    if isOperator(temp) then b <- apply temp
    elif System.Char.IsLetter(temp.[0]) then 
      b <- vals.[List.findIndex ((=) temp) vars]
    elif temp.Length > 1 && temp.[0] = '-' && 
         System.Char.IsLetter(temp.[1]) then
      b <- -vals.[List.findIndex ((=) temp.[1..temp.Length - 1]) vars]
    else b <- int temp

    match operator with
    | "+" -> b + a
    | "-" -> b - a
    | "*" -> b * a
    | "/" -> b / a
    | "%" -> b % a 
    | "^" -> pown b a
    | _   -> failwith "Invalid operator"
      
  apply (opstack.Pop())

 
   


[<TestCase ("0 + 1",             Result = 1)>]
[<TestCase ("5 + (-5)",          Result = 0)>]
[<TestCase ("999999999 + 1",     Result = 1000000000)>]
[<TestCase ("1 - 15",            Result = -14)>]
[<TestCase ("1 - 2 - 3",         Result = -4)>]
[<TestCase ("6 / 2",             Result = 3)>]
[<TestCase ("123 % 10",          Result = 3)>]
[<TestCase ("89 * 3",            Result = 267)>]
[<TestCase ("678 ^ 0",           Result = 1)>]
[<TestCase ("2 ^ 1 + 3",         Result = 5)>]
[<TestCase ("3 ^ 2 ^ 2",         Result = 81)>]
[<TestCase ("3 ^ 1 ^ 2",         Result = 3)>]
[<TestCase ("2 + 2 * 2",         Result = 6)>]
[<TestCase ("(2 + 2) * 2",       Result = 8)>]
[<TestCase ("((1 + 1) * 3) ^ 3", Result = 216)>]
[<TestCase ("(3 + 8 * 2 / (1 - 5) ^ 2) * 2", Result = 8)>]
[<TestCase ("0 * 1 + 1 + 1 * 0",             Result = 1)>]
[<TestCase ("7 + 6 - 5 * (4 / (3 % 2 ^ 1))", Result = -7)>]
let ``Simple calc tests`` e =
    calculate e [] []
   


[<TestFixture>]
type ``Calc with variables`` () = 
  [<Test>]
  member this.``Test 1`` () = 
    (calculate "a + b" ["a"; "b"] [1; 2]) |> should equal 3

  [<Test>]
  member this.``Test 2`` () = 
    (calculate "a * b" ["a"; "b"] [4; 6]) |> should equal 24
  
  [<Test>]
  member this.``Test `` () = 
    (calculate "(6 + LOL) * 2" ["LOL"] [2]) |> should equal 16

  [<Test>]
  member this.``Test`` () = 
    (calculate "(6 + (-LOL)) * 2" ["LOL"] [2]) |> should equal 8




[<EntryPoint>]
let main argv = 
  printfn "%A" (argv)
  0