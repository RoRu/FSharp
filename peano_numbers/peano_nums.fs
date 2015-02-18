// Peano numbers by Vladimir Yumatov
// SPBSU 171 gr. 
type Peano = Zero | S of Peano

let rec pnSum a b = 
  match a with
  | Zero -> b
  | S a -> S (pnSum a b)

let pnMinus1 a = 
  match a with 
  | Zero -> Zero
  | S a -> a

let rec pnSub a b = 
  match a, b with
  | Zero, _ -> Zero
  | _, Zero -> a
  | S a, S b -> (pnSub a b)

let rec pnMult a b = 
  match a, b with
  | Zero, _ -> Zero
  | _, Zero -> Zero
  | S a, b -> pnSum b (pnMult a b)

let rec pnPow a b = 
  match a, b with
  | Zero, _ -> Zero
  | _, Zero -> S Zero
  | a, S b -> pnMult (pnPow a b) a

let rec pnToInt a = 
  match a with
  | Zero -> 0
  | S a -> pnToInt a + 1

let rec intToPn n = 
  match n with
  | 0 -> Zero
  | n -> pnSum (intToPn (n - 1)) (S Zero)

[<EntryPoint>]
let main argv = 
    printfn "%A\n" (intToPn 4)
    0    