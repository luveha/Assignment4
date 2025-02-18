module Interpreter.Eval

    open Result
    open Language
    open State
    
    let apply (n1: 'a option) (n2: 'a option) (operation: 'a -> 'a -> 'b)= 
        match n1,n2 with
        |  Some x, Some y -> Some (operation x y)
        | _ -> None

    let rec arithEval a st = 
        match a with
        | Num n -> Ok n
        | Var v when v = "random" -> Ok 7 //7 is always random
        | Var v ->
                getVar v st
        | Add (x,y) ->
                match arithEval x st, arithEval y st with
                | Ok x, Ok y -> Ok (x + y)
                | Error e, _ | _ , Error e-> Error(e)
        | Mul (x,y) ->
                match arithEval x st, arithEval y st with
                | Ok x, Ok y -> Ok (x * y)
                | Error e, _ | _ , Error e-> Error(e)
        | Div (x,y) ->
                match arithEval x st, arithEval y st with
                | Error e, _ | _ , Error e-> Error(e)
                | Ok x, Ok 0 -> Error (DivisionByZero) 
                | Ok x, Ok y -> Ok (x / y)
        | Mod (x,y)-> 
                match arithEval x st, arithEval y st with
                | Error e, _ | _ , Error e-> Error(e)
                | Ok x, Ok 0 -> Error (DivisionByZero) 
                | Ok x, Ok y -> Ok (x % y)                

    let rec boolEval b st = 
        match b with
        | TT -> Ok true
        | Eq (a,c)-> 
                match arithEval a st, arithEval c st with
                | Error e, _ | _, Error e -> Error(e)
                | Ok x,Ok y -> Ok (x = y)  
        | Lt (a,c) -> 
                match arithEval a st, arithEval c st with
                | Error e, _ | _, Error e -> Error(e)
                | Ok x,Ok y -> Ok (x < y) 
        | Conj (a,c) -> 
                match boolEval a st, boolEval c st with
                | Error e, _ | _, Error e -> Error(e)
                | Ok x,Ok y -> Ok (x && y) 
        | Not a -> 
                match boolEval a st with
                | Error e -> Error(e)
                | Ok x -> Ok (not x)

    let rec stmntEval s st = 
        match s with 
        | Skip -> Ok st
        | Declare s -> declare s st
        | Assign(v,a) -> 
                match arithEval a st with
                | Error e-> Error e
                | Ok x -> setVar v x st
        | Seq (s1,s2) -> 
                match stmntEval s1 st with
                | Error e -> Error e
                | Ok st' -> 
                        match stmntEval s2 st' with
                        | Error e -> Error e
                        | st'' -> st'' 
        | If (gaurd, s1, s2) -> 
                match boolEval gaurd st with
                | Error e -> Error e
                | Ok true -> stmntEval s1 st
                | Ok false -> stmntEval s2 st
        | While (gaurd, s') -> 
                match boolEval gaurd st with
                | Error e -> Error e
                | Ok true -> 
                        match stmntEval s' st with
                        | Error e -> Error e
                        | Ok st'' -> stmntEval (While (gaurd, s')) st''
                | Ok false -> Ok st