module Interpreter.State

    open Result
    open Language
    

    let reservedVariableNameList = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
    let reservedVariableName s= List.exists(fun x -> x = s) reservedVariableNameList

    let validVariableName (s: string) = 
        match s.[0] with
        | c when System.Char.IsAsciiLetter(c) || c = '_' -> String.forall(fun c-> System.Char.IsAsciiLetterOrDigit(c) || c = '_') s.[1..]
        | _ -> false
    
    
    type state = {m: Map<string,int>}
    let mkState () = {m = Map.empty<string,int>}

    let declare x st = 
        match st.m with
        | m when m.ContainsKey x -> Error (VarAlreadyExists(x))
        | _ when reservedVariableName x -> Error (ReservedName(x))
        | _ when not (validVariableName x) -> Error (InvalidVarName(x))
        | _ -> Ok {m = st.m.Add(x,0)}

    let getVar x st = 
        match st.m.ContainsKey x with
        | true -> Ok st.m.[x]
        | _ -> Error (VarNotDeclared(x))

    let setVar x v st = 
        match st.m.ContainsKey x with
        | true -> Ok {m = st.m.Add(x,v)}
        | _ -> Error (VarNotDeclared(x))


    let random _ = failwith "not implemented"
        
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     