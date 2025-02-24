module main
    open Interpreter.State
    open Interpreter.Eval
    open Interpreter.Programs
    open Interpreter.Language

    [<EntryPoint>]
    let main argv =

        let s0 = () |> mkState |> declare "x" |> Result.bind (declare "x") |> Result.bind (setVar "x" 42) |> Result.bind (getVar "x")
        let s1 = () |> mkState |> stmntEval (factorial_err1 5) |> Result.bind (getVar "result")   
 
        let g0 = () |>mkState |>declare "if" |>Result.bind (getVar "if")
    
        let f0 = () |>mkState |>declare "x" |>Result.bind (setVar "x" 42) |> Result.bind (boolEval (Var "y" .<=. Num 41))
        let f1 = () |>mkState |>declare "x" |>Result.bind (getVar "y")


        printf "%A\n" s0
        printf "%A\n" s1
        printf "\n" 
        printf "%A\n" f0
        printf "%A\n" f1
        printf "\n" 
        printf "%A\n" g0

       (*  printfn "reservedVariableName works: %b" (not (reservedVariableName "hello") && (reservedVariableName "if") && (reservedVariableName "__result__"))
        printfn "validVariableName works: %b" (validVariableName "_hello_1" && not (validVariableName "1_hello")) *)

        (* printfn "%A" (() |> mkState |> getVar "x")
        printfn "%A" (() |> mkState |> declare "x" |> bind (getVar "x"))
        printfn "%A" (() |> mkState |> declare "x" |> bind (setVar "x" 42) |> bind (getVar "x"))
        printfn "%A" (() |> mkState |> declare "1x" |> bind (setVar "1x" 42) |> bind (getVar "1x")) *)
        
        0 // return an integer exit code