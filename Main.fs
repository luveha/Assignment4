module main
    open Interpreter.State
    open Option

    [<EntryPoint>]
    let main argv =

        printfn "reservedVariableName works: %b" (not (reservedVariableName "hello") && (reservedVariableName "if") && (reservedVariableName "__result__"))
        printfn "validVariableName works: %b" (validVariableName "_hello_1" && not (validVariableName "1_hello"))

        printfn "%A" (() |> mkState |> getVar "x")
        printfn "%A" (() |> mkState |> declare "x" |> bind (getVar "x"))
        printfn "%A" (() |> mkState |> declare "x" |> bind (setVar "x" 42) |> bind (getVar "x"))
        printfn "%A" (() |> mkState |> declare "1x" |> bind (setVar "1x" 42) |> bind (getVar "1x"))

        0 // return an integer exit code