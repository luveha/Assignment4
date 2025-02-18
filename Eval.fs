module Interpreter.Eval

    open Result
    open Language
    open State
    
    let apply (n1: 'a option) (n2: 'a option) (operation: 'a -> 'a -> 'a)= 
        match n1,n2 with
        |  Some x, Some y -> Some (operation x y)
        | _ -> None

    let rec arithEval (a: aexpr) (st: state) = 
        match a with
        | Num n -> Some n
        | Var v when v = "random" -> Some 7 //7 is always random
        | Var v when st.m.ContainsKey v -> Some st.m.[v]
        | Add (x,y) ->
                apply (arithEval x st) (arithEval y st) (fun x y -> x+y)
        | Mul (x,y) -> 
                apply (arithEval x st) (arithEval y st) (fun x y -> x*y)
        | Div (x,y) | Mod (x,y)-> 
                match arithEval y st with
                | Some yInt when not (yInt = 0) -> 
                        match a with
                        | Div _ -> apply (arithEval x st) (arithEval y st) (fun x y -> x/y)
                        | _ -> apply (arithEval x st) (arithEval y st) (fun x y -> x%y)
                | _ -> None
        | _ -> None
    let rec boolEval (b: bexpr) (st: state) = 
        match b with
        | TT -> Some true
        | FF -> Some false
        | Eq(a,c) -> 
                apply (arithEval a st) (arithEval c st) (fun (x:int) (y:int) -> x - y)
        | _ -> Some false
    let stmntEval _ = failwith "not implemented"