module Interpreter.State

    open Result
    open Language
    

    let reservedVariableNameList = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
    let reservedVariableName (s:string) = List.exists(fun x -> x = s) reservedVariableNameList

    let validVariableName _ = failwith "not implemented"
    
    type state = unit // your type goes here
    
    let mkState _ = failwith "not implemented"
    let random _ = failwith "not implemented"
    
    let declare _ = failwith "not implemented"
    
    let getVar _ = failwith "not implemented"
    let setVar _ = failwith "not implemented"
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     