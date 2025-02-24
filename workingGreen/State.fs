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
        | m when m.ContainsKey x -> None
        | _ when not (validVariableName x) || reservedVariableName x -> None
        | _ -> Some {m = st.m.Add(x,0)}

    let getVar x st = 
        match st.m.ContainsKey x with
        | true -> Some st.m.[x]
        | _ -> None

    let setVar x v st = 
        match st.m.ContainsKey x with
        | true -> Some {m = st.m.Add(x,v)}
        | _ -> None


    let random _ = failwith "not implemented"
        
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     