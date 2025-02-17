module main
    open Interpreter.State

    printfn "%b" (reservedVariableName "hello")

    printfn "%b" (reservedVariableName "if")

    printfn "%b" (reservedVariableName "__result__")