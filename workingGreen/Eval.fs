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

    let rec arithEval2 a st = 
        match a with
        | Num n -> Some n
        | Var v when v = "random" -> Some 7
        | Var v when st.m.ContainsKey v -> Some st.m.[v]
        | Add (x, y) ->
            arithEval2 x st
            |> Option.bind (fun x -> arithEval2 y st |> Option.map (fun y -> x + y))
        | Mul (x, y) ->
            arithEval2 x st
            |> Option.bind (fun x -> arithEval2 y st |> Option.map (fun y -> x * y))
        | Div (x, y) | Mod (x, y) ->
            arithEval2 y st
            |> Option.bind (fun yInt ->
                if yInt = 0 then None
                else
                    arithEval2 x st |> Option.map (fun x ->
                        match a with
                        | Div _ -> x / yInt
                        | _ -> x % yInt))
        | _ -> None


    let rec boolEval b st = 
        match b with
        | TT -> Some true
        | Eq (a,c)-> 
                apply (arithEval a st) (arithEval c st) (fun x y -> x = y)
        | Lt (a,c) -> 
                apply (arithEval a st) (arithEval c st) (fun x y -> x < y)
        | Conj (a,c) -> 
                apply (boolEval a st) (boolEval c st) (fun x y -> x && y)
        | Not a -> apply (boolEval a st) (Some false) (fun x y -> not x)

    let rec stmntEval s st = 
        match s with 
        | Skip -> Some st
        | Declare s -> declare s st
        | Assign(v,a) -> 
                match arithEval a st with 
                | Some x -> setVar v x st
                | None -> None
        | Seq (s1,s2) -> 
                match stmntEval s1 st with
                | Some st' -> 
                        match stmntEval s2 st' with
                        | None -> None
                        | st'' -> st'' 
                | _ -> None 
        | If (gaurd, s1, s2) -> 
                match boolEval gaurd st with
                | Some true -> stmntEval s1 st
                | Some false -> stmntEval s2 st
                | None -> None
        | While (gaurd, s') -> 
                match boolEval gaurd st with
                | Some true -> 
                        match stmntEval s' st with
                        | Some st'' -> stmntEval (While (gaurd, s')) st''
                        | None -> None
                | Some false -> Some st
                | None -> None