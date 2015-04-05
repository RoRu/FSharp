// Tasks 37-38. Calc with file input/output
// by Vladimir Yumatov
// SPBU, group 171

// Expected execution time: 1.5 h
// Real time: 1.5 h 

open System.IO
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

    member this.GetList () = st

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
        while ostc.Length > 0 && prior(ostc.Top()) >= prior(ch)
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
let calculate (opstack: Stack<string>) = 
  //let opstack = translate exp
  //printfn "\n%A" opstack

  let rec apply operator =
    let mutable a = 0
    let mutable b = 0
    let mutable temp = opstack.Pop()
    let isOperator (a : string) = 
      a.Length = 1 && prior(a.[0]) > 0

    if isOperator(temp) then a <- apply temp
    //elif System.Char.IsLetter(temp.[0]) then 
      //a <- vals.[List.findIndex ((=) temp) vars]
    //elif temp.Length > 1 && temp.[0] = '-' &&
      //   System.Char.IsLetter(temp.[1]) then
      //a <- -vals.[List.findIndex ((=) temp.[1..temp.Length - 1]) vars]
    else a <- int temp
    temp <- opstack.Pop()
    if isOperator(temp) then b <- apply temp
    //elif System.Char.IsLetter(temp.[0]) then 
      //b <- vals.[List.findIndex ((=) temp) vars]
    //elif temp.Length > 1 && temp.[0] = '-' && 
      //   System.Char.IsLetter(temp.[1]) then
      //b <- -vals.[List.findIndex ((=) temp.[1..temp.Length - 1]) vars]
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
        | p' -> elm * (pow elm (p' - 1))
      if a >= 0 then pow b a
      else 1 / (pow b (-a))
    | _   -> failwith "Invalid operator"
      
  apply (opstack.Pop())

 
// Task 37
let writeVert (fin: StreamReader) (fout: StreamWriter) = 
  let st = translate (fin.ReadLine())
  let lst = List.rev (st.GetList())
  for i in 0..lst.Length - 1 do
    if i = 0 then fout.Write(lst.[i])
    else fout.Write("\n" + lst.[i])


// Task 38
let calcVert (fin: StreamReader) (fout: StreamWriter) = 
  let st = Stack<string>()
  let mutable op = ""
  while not fin.EndOfStream do
    op <- fin.ReadLine()
    st.Push(op)
  fout.Write(string (calculate st))


[<TestCase ("0 + 1",             Result = "0\n1\n+")>]
[<TestCase ("3 ^ 2 ^ 2",         Result = "3\n2\n^\n2\n^")>]
[<TestCase ("2 + 2 * 2",         Result = "2\n2\n2\n*\n+")>]
[<TestCase ("7 * (6 + 5)",       Result = "7\n6\n5\n+\n*")>]
[<TestCase ("7 * 6 + 5",         Result = "7\n6\n*\n5\n+")>]
let ``Task 37`` (e: string) =
  let w = new StreamWriter("test.in") 
  w.Write(e)
  w.Dispose()
  let sout = new StreamWriter("test.out")
  let sin = new StreamReader("test.in")
  writeVert sin sout
  sin.Dispose()
  sout.Dispose()
  let t = new StreamReader("test.out")
  let res = t.ReadToEnd()
  t.Dispose()
  res



[<TestCase ("5 + (-5)",          Result = "0")>]
[<TestCase ("999999999 + 1",     Result = "1000000000")>]
[<TestCase ("1 - 15",            Result = "-14")>]
[<TestCase ("6 / 2",             Result = "3")>]
[<TestCase ("123 % 10",          Result = "3")>]
[<TestCase ("89 * 3",            Result = "267")>]
[<TestCase ("678 ^ 0",           Result = "1")>]
[<TestCase ("2 ^ 1 + 3",         Result = "5")>]
[<TestCase ("3 ^ 2 ^ 2",         Result = "81")>]
[<TestCase ("2 + 2 * 2",         Result = "6")>]
let ``Task 38`` (e: string) = 
  let w = new StreamWriter("test.in") 
  w.Write(e)
  w.Dispose()
  let sout = new StreamWriter("test.out")
  let sin = new StreamReader("test.in")
  writeVert sin sout
  sin.Dispose()
  sout.Dispose()

  let sin' = new StreamReader("test.out")
  let sout' = new StreamWriter("test2.out")
  calcVert sin' sout'
  sin'.Dispose()
  sout'.Dispose()
  let t = new StreamReader("test2.out")
  let res = t.ReadLine()
  t.Dispose()
  res




[<EntryPoint>]
let main argv = 
  let sin = new StreamWriter("test.in")
  let sout = new StreamReader("test.out")
  //writeVert sout sin
  //sin.Dispose()
  //sout.Dispose()
  //let t = new StreamReader("test.in")
  //printfn "%s" (t.ReadToEnd()) 
  //t.Dispose()
  (calcVert sout sin)
  sin.Dispose()
  sout.Dispose()
  0