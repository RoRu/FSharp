// Tasks 42-43. Scientific calc with GUI
// by Vladimir Yumatov
// SPBU, group 171

// Expected time: 3.5 h
// Real time: 5 h 

module Calc

open System.Windows.Forms
open System.Drawing

let mutable operandL = ""
let mutable operandR = ""
let mutable operator = ""
let mutable opFlag   = false

let exprInput = 
  let tbox = new TextBox()
  tbox.Location <- Point(10, 10)
  tbox.Width <- 250
  tbox.Font <- new Font("Arial", 13.0f)
  tbox.Text <- "0"
  tbox


let butNums n = 
  let but = new Button()
  but.Size <- Size(30, 30)
  but.Text <- n.ToString()
  match n with
  | 0 -> but.Location <- Point(45, 150)
  | 1 | 2 | 3 -> but.Location <- Point(35 * (n - 1) + 10, 45)
  | 4 | 5 | 6 -> but.Location <- Point(35 * (n - 4) + 10, 80)
  | 7 | 8 | 9 -> but.Location <- Point(35 * (n - 7) + 10, 115)
  | _ -> ()

  but.Click.Add (fun _ -> 
    let s = exprInput.Text
    if (s.[0] >= '0' && s.[0] <= '9' && s <> "0") || (s.[0] = '-' && s <> "-") then
      exprInput.Text <- s + n.ToString()
    else
      exprInput.Text <- n.ToString()
      opFlag <- false
  )
  but


let butErase = 
  let but = new Button()
  but.Size <- Size(40, 25)
  but.Text <- "←"
  but.Location <- Point(270, 10)
  but.Click.Add (fun _ ->
    let s = exprInput.Text
    if s.Length > 1 then
      exprInput.Text <- s.[0..s.Length - 2]
    elif s.Length = 1 then
      exprInput.Text <- "0"
  )
  but


let butOperator (op: string) = 
  let but = new Button()
  but.Size <- Size(30, 30)
  but.Text <- op

  match op with
  | "+" -> but.Location <- Point(120, 45)
  | "-" -> but.Location <- Point(120, 80)
  | "*" -> but.Location <- Point(120, 115)
  | "/" -> but.Location <- Point(120, 150)
  | "%" -> but.Location <- Point(155, 45)
  | "^" -> but.Location <- Point(155, 80)
  | _ -> ()

  but.Click.Add(fun _ ->
    opFlag <- true

    //printfn "%A   %A   %A" operandL operator operandR
    if operator = "" then
      operandL <- exprInput.Text
    operator <- op
    exprInput.Text <- op
    //printfn "%A   %A   %A" operandL operator operandR
  )
  but


let butEq = 
  let but = new Button()
  but.Size <- Size(100, 30)
  but.Location <- Point(10, 185)
  but.Text <- "="
  but.Font <- new Font("Arial", 16.0f)
  but.Click.Add (fun _ ->
    if operator = "" then ()
    else
      operandR <- exprInput.Text
      //printfn "%A   %A   %A" operandL operator operandR
      match operator with
      | "+" -> exprInput.Text <- 
               (float operandL + float operandR).ToString()
      | "-" -> exprInput.Text <- 
               (float operandL - float operandR).ToString()
      | "*" -> exprInput.Text <- 
               (float operandL * float operandR).ToString()
      | "/" -> exprInput.Text <- 
               (float operandL / float operandR).ToString()
      | "%" -> exprInput.Text <- 
               (float operandL % float operandR).ToString()
      | "^" -> exprInput.Text <- 
               (float operandL ** float operandR).ToString() 
      | _ -> failwith "Invalid operator"
      operator <- ""
      operandL <- ""
      operandR <- ""
      opFlag <- false
      //printfn "%A   %A   %A" operandL operator operandR
  )
  but



let butNeg = 
  let but = new Button()
  but.Size <- Size(30, 30)
  but.Text <- "±"
  but.Location <- Point(155, 115)
  but.Click.Add (fun _ ->
    //printfn "%A" (float exprInput.Text)
    if (System.Char.IsDigit(exprInput.Text.[0]) && exprInput.Text <> "0") ||
       (exprInput.Text.[0] = '-' && exprInput.Text <> "-") then
      exprInput.Text <- ((float exprInput.Text) * (-1.0)).ToString()
    //printfn "%A" (float exprInput.Text)
  )
  but



let butSqrt =
  let but = new Button()
  but.Text <- "√"
  but.Size <- System.Drawing.Size(30, 30)
  but.Location <- System.Drawing.Point(155, 150)
  but.Click.Add (fun _ ->
    exprInput.Text <- (sqrt(float exprInput.Text)).ToString()
  )
  but



let butSin = 
  let but = new Button()
  but.Text <- "Sin"
  but.Size <- System.Drawing.Size(35, 30)
  but.Location <- System.Drawing.Point(190, 45)
  but.Click.Add (fun _ ->
    exprInput.Text <- (sin(float exprInput.Text)).ToString()
  )
  but

let butCos = 
  let but = new Button()
  but.Text <- "Cos"
  but.Size <- System.Drawing.Size(35, 30)
  but.Location <- System.Drawing.Point(190, 80)
  but.Click.Add (fun _ ->
    exprInput.Text <- (cos(float exprInput.Text)).ToString()
  )
  but

let butTan = 
  let but = new Button()
  but.Text <- "Tan"
  but.Size <- System.Drawing.Size(35, 30)
  but.Location <- System.Drawing.Point(190, 115)
  but.Click.Add (fun _ ->
    exprInput.Text <- (tan(float exprInput.Text)).ToString()
  )
  but

let butCot = 
  let but = new Button()
  but.Text <- "Cot"
  but.Size <- System.Drawing.Size(35, 30)
  but.Location <- System.Drawing.Point(190, 150)
  but.Click.Add (fun _ ->
    exprInput.Text <- (1.0 / tan(float exprInput.Text)).ToString()
  )
  but



let butLn = 
  let but = new Button()
  but.Text <- "ln"
  but.Size <- System.Drawing.Size(35, 30)
  but.Location <- System.Drawing.Point(230, 45)
  but.Click.Add (fun _ ->
    exprInput.Text <- (log(float exprInput.Text)).ToString()
  )
  but

let butLg = 
  let but = new Button()
  but.Text <- "lg"
  but.Size <- System.Drawing.Size(35, 30)
  but.Location <- System.Drawing.Point(230, 80)
  but.Click.Add (fun _ ->
    exprInput.Text <- (log(float exprInput.Text) / 
                       log 10.0).ToString()
  )
  but



let butRev = 
  let but = new Button()
  but.Text <- "1/x"
  but.Size <- System.Drawing.Size(35, 30)
  but.Location <- System.Drawing.Point(230, 115)
  but.Click.Add (fun _ ->
    exprInput.Text <- (1.0 / (float exprInput.Text)).ToString()
  )
  but



let butPi = 
  let but = new Button()
  but.Size <- Size(30, 30)
  but.Location <- Point(120, 185)
  but.Text <- "π"
  but.Click.Add (fun _ ->
    exprInput.Text <- (System.Math.PI).ToString()
  )
  but

let butE = 
  let but = new Button()
  but.Size <- Size(30, 30)
  but.Location <- Point(155, 185)
  but.Text <- "e"
  but.Click.Add (fun _ ->
    exprInput.Text <- (System.Math.E).ToString()
  )
  but



let butDot = 
  let but = new Button()
  but.Size <- Size(30, 30)
  but.Text <- "."
  but.Location <- Point(80, 150)
  but.Click.Add (fun _ ->
    if (not(exprInput.Text.Contains(".")) && 
        System.Char.IsDigit(exprInput.Text.[0])) then
      exprInput.Text <- exprInput.Text + "."
  )
  but

let butClear = 
  let but = new Button()
  but.Size <- Size(30, 30)
  but.Text <- "C"
  but.Location <- Point(10, 150)
  but.Click.Add (fun _ ->
    exprInput.Text <- "0"
    operator <- ""
    operandL <- ""
    operandR <- ""
  )
  but



let mainForm = 
  let fm = new Form(Visible = false)
  fm.ClientSize <- Size(320, 230)

  for n in 0..9 do
    fm.Controls.Add(butNums n)
  for s in ["+"; "-"; "*"; "/"; "%"; "^"] do
    fm.Controls.Add(butOperator s) 
  fm.Controls.Add(exprInput)
  fm.Controls.Add(butDot)
  fm.Controls.Add(butClear)
  fm.Controls.Add(butEq)
  fm.Controls.Add(butNeg)
  fm.Controls.Add(butSqrt)
  fm.Controls.Add(butErase)
  fm.Controls.Add(butPi)
  fm.Controls.Add(butE)
  fm.Controls.Add(butSin)
  fm.Controls.Add(butCos)
  fm.Controls.Add(butTan)
  fm.Controls.Add(butCot)
  fm.Controls.Add(butLn)
  fm.Controls.Add(butLg)
  fm.Controls.Add(butRev)

  fm


[<EntryPoint>]
let main argv = 
  mainForm.Visible <- true
  Application.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  Application.Run()
  0 