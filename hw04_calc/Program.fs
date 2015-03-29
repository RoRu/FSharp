// Task 35. Calculator and tests for it
// by Vladimir Yumatov
// SPBU, group 171

// Expected execution time: 3-3.5 h
// Real time: 5.5 h 


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


let isOperator (a : string) = 
  if a.Length = 1 && prior(a.[0]) > 0 then true else false


// translates string to an actual expression in postfix notation 
// and divides it into tokens
let translate (exp : string) =
  let stc = Stack<char>()
  let res = Stack<string>()

  let mutable temp = ""
  for i in 0..exp.Length - 1 do
    let t = exp.[i]
    if System.Char.IsDigit(t) then 
      temp <- temp + t.ToString()
    else
      match t with
      | ' ' ->
        if System.Char.IsDigit(exp.[i - 1]) then
          res.Push(temp)
          temp <- ""
      | '(' -> stc.Push(t)
      | ')' ->
        if temp.Length > 0 then
          res.Push(temp)
          temp <- "" 
        while stc.Top() <> '(' && stc.Length > 0 do
          res.Push(stc.Pop().ToString())
        ignore(stc.Pop())
      | _   ->
        if t = '-' && System.Char.IsDigit(exp.[i + 1]) then 
          temp <- "-"
        else
          while stc.Length > 0 && 
                ((prior(stc.Top()) >= prior(t) && prior(t) < 3) || 
                 (prior(stc.Top()) >  prior(t) && prior(t) = 3)) 
            do res.Push(stc.Pop().ToString())
          stc.Push(t)
      
  if temp.Length > 0 then
    res.Push(temp)
    temp <- "" 
  while stc.Length > 0 do 
    res.Push(stc.Pop().ToString())
  res

// returns result of the expression
let calculate exp =
  let opstack = translate(exp)
  printfn "\n%A" opstack

  let rec apply operator =
    let mutable a = 0
    let mutable b = 0
    let mutable temp = opstack.Pop()

    if isOperator(temp) then a <- apply temp
    else a <- int temp
    temp <- opstack.Pop()
    if isOperator(temp) then b <- apply temp
    else b <- int temp

    match operator with
    | "+" -> b + a
    | "-" -> b - a
    | "*" -> b * a
    | "/" -> b / a
    | "%" -> b % a 
    | "^" ->
      let rec pow elm p =
        match p with
        | 0 -> 1
        | 1 -> elm
        | p -> elm * (pow elm (p - 1))
      if a >= 0 then pow b (int a)
      else 1 / (pow b (-int a))
    | _   -> failwith "Invalid operator"
      
  apply (opstack.Pop())


[<TestCase ("0 + 1",             Result = 1)>]
[<TestCase ("5 + (-5)",          Result = 0)>]
[<TestCase ("999999999 + 1",     Result = 1000000000)>]
[<TestCase ("1 - 15",            Result = -14)>]
[<TestCase ("6 / 2",             Result = 3)>]
[<TestCase ("123 % 10",          Result = 3)>]
[<TestCase ("89 * 3",            Result = 267)>]
[<TestCase ("678 ^ 0",           Result = 1)>]
[<TestCase ("3 ^ 2 ^ 2",         Result = 81)>]
[<TestCase ("2 + 2 * 2",         Result = 6)>]
[<TestCase ("(2 + 2) * 2",       Result = 8)>]
[<TestCase ("((1 + 1) * 3) ^ 3", Result = 216)>]
[<TestCase ("(3 + 8 * 2 / (1 - 5) ^ 2) * 2", Result = 8)>]
[<TestCase ("0 * 1 + 1 + 1 * 0",             Result = 1)>]
[<TestCase ("7 + 6 - 5 * (4 / (3 % 2 ^ 1))", Result = -7)>]
let ``Simple calc tests`` e =
    calculate e
   


[<EntryPoint>]
let main argv = 
  calculate "6 / 2" |> ignore
  0