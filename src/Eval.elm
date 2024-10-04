module Eval exposing (..)

import Model exposing (..)
import Syntax exposing (..)
import LangUtils exposing (..)
import Round

eval :  VEnv -> Expr -> ExprNode
eval venv expr =
    case expr of

        EVar _ s ->
            let
                v = 
                    case findVarByName s venv of
                        Just val ->
                            case val of
                                VFix e -> 
                                    let
                                        en = eval venv (EFix defaultWS e) 
                                    in
                                        getValueFromExprNode en

                                _    -> val

                        Nothing -> VError "Variable Error: 01"
            in
                (ECVar, expr, { value = v })


        ELam _ p e ->
            let
                v = VClosure p e venv
            in
                (ECLam, expr, { value = v })
            

        ELet _ p e1 e2 ->
            eval venv (EApp defaultWS (ELam defaultWS p e2) e1)          
            

        ELetrec _ p e1 e2 ->
            eval venv 
                (EApp defaultWS (ELam defaultWS p e2) (EFix defaultWS (ELam defaultWS p e1)))


        EApp _ e1 e2 ->
            let
                en1 = eval venv e1
                v1 = getValueFromExprNode en1
            in
                case v1 of

                    VClosure p ef venvf ->
                        case e2 of
                            EFix _ e -> 
                                case p of
                                    PVar _ s -> 
                                        let
                                            v2 = VFix e
                                            en2 = (ECFix, e2, { value = v2 })

                                            en3 = eval ((s, v2)::venvf) ef
                                            v3 = getValueFromExprNode en3
                                        in
                                            (ECApp en1 en2 en3, expr, { value = v3 })                                        

                                    _      -> (ECError, expr, { value = VError "Recursion Error: 01" })
                            _ ->  
                                let 
                                    en2 = eval venv e2
                                    v2 = getValueFromExprNode en2
                                
                                    venvm = match p v2
                                in
                                    case venvm of
                                        [(_, VError info)] -> (ECError, expr, { value = VError info})
                                        _                  -> 
                                            let
                                                en3 = eval (venvm++venvf) ef
                                                v3 = getValueFromExprNode en3
                                            in
                                                (ECApp en1 en2 en3, expr, { value = v3 })
                                            

                    _ -> (ECError, expr, { value = VError <| "Function Error: 01\\r" ++ Debug.toString v1 }) 


        EInt _ n ->
            let 
                v = VInt n
            in
                (ECInt, expr, { value = v })


        EFloat _ n ->
            let 
                v = VFloat n
            in
                (ECFloat, expr, { value = v })
        

        ETrue _ -> 
            let
                v = VTrue
            in
                (ECTrue, expr, { value = v })


        EFalse _ -> 
            let
                v = VFalse
            in
                (ECFalse, expr, { value = v })


        EChar _ c -> 
            let
                v = VChar c
            in
                (ECChar, expr, { value = v })


        ECons (_, id) e1 e2 ->
            let 
                en1 = eval venv e1
                v1 = getValueFromExprNode en1

                en2 = eval venv e2
                v2 = getValueFromExprNode en2

                v = 
                    if id == eoCons then
                        -- handle (::) operation on string
                        case v2 of
                            VCons 1 _ _ ->
                                case v1 of
                                    VChar _ -> VCons vsId v1 v2

                                    _ -> VError ("Cons (::) Error: cannot cons other type except char with string" ++ (print v1) )
                            
                            VNil 1 ->
                                case v1 of
                                    VChar _ -> VCons vsId v1 v2

                                    _ -> VError ("Cons (::) Error: cannot cons other type except char with string" ++ (print v1) )
                            
                            _ -> 
                                VCons voId v1 v2
                    else 
                        if id == esQuo || id == esElm then 
                            VCons vsId v1 v2
                        else 
                            VCons voId v1 v2
            in 
                (ECCons en1 en2, expr, { value = v })
                

        EBTuple _ e1 e2 ->
            let 
                en1 = eval venv e1
                v1 = getValueFromExprNode en1
                
                en2 = eval venv e2
                v2 = getValueFromExprNode en2
                
                v = VBTuple v1 v2
            in
                (ECBTuple en1 en2, expr, { value = v })


        ETTuple _ e1 e2 e3 ->
            let 
                en1 = eval venv e1
                v1 = getValueFromExprNode en1

                en2 = eval venv e2
                v2 = getValueFromExprNode en2

                en3 = eval venv e3 
                v3 = getValueFromExprNode en3

                v = VTTuple v1 v2 v3
            in
                (ECTTuple en1 en2 en3, expr, { value = v })
        

        ENil (_, id) ->
            let
                v = 
                    if id == esQuo || id == esElm then
                        VNil 1
                    else 
                        VNil 0
            in
                (ECNil, expr, { value = v })

        
        EDictDef _ dictPairs -> 
            let
                (ecDictPairs, vDictPairs) = evalDictPairs venv dictPairs
                v = VDict vDictPairs
            in
                (ECDictDef ecDictPairs, expr, { value = v })
        
        EDictUpd _ originDict dictPairs ->
            let
                enOriginDict = eval venv originDict
                (ecDictPairs, vDictPairs) = evalDictPairs venv dictPairs
                v = vDictUpdates (getValueFromExprNode enOriginDict) vDictPairs
            in
                (ECDictUpd enOriginDict ecDictPairs, expr, { value = v })

        EField _ de fstr ->
            let
                enDict = eval venv de
                v = fieldAccess (getValueFromExprNode enDict) fstr
            in
                (ECField enDict, expr, { value = v })


        EUPrim _ op e ->
            let 
                en1 = eval venv e
                v1 = getValueFromExprNode en1

                v = 
                    case op of
                    Neg ->
                        case v1 of
                            VInt n    ->
                                VInt (0-n)
                            VFloat n  ->
                                VFloat (0-n)
                            _         ->
                                VError "Arithmetic Error: 01"
                    Not ->
                        case v1 of
                            VTrue     ->
                                VFalse
                            VFalse    ->
                                VTrue
                            _         ->
                                VError "Logical Operation Error: 01"
            in
                (ECUPrim en1, expr, { value = v })

        EBPrim _ op e1 e2 ->
            let 
                en1 = eval venv e1
                v1 = getValueFromExprNode en1

                en2 = eval venv e2
                v2 = getValueFromExprNode en2
                
                v = 
                    case v1 of
                        VInt n1 ->
                            case v2 of
                                VInt n2 -> 
                                    case op of
                                        Add -> VInt (n1 + n2)
                                        Sub -> VInt (n1 - n2)
                                        Mul -> VInt (n1 * n2)
                                        Div -> VInt (n1 // n2)
                                        DDiv -> VFloat (Round.roundNumCom 2 
                                                        <| ((toFloat n1) / (toFloat n2)))
                                        Eq -> boolOp (n1 == n2)
                                        Ne -> boolOp (n1 /= n2)
                                        Lt -> boolOp (n1 < n2)
                                        Gt -> boolOp (n1 > n2)
                                        Le -> boolOp (n1 <= n2)
                                        Ge -> boolOp (n1 >= n2)
                                        _  -> 
                                            VError "Logical Operation Error: 02"
                                
                                VFloat n2 ->
                                    floatOp op (toFloat n1) n2

                                _         ->
                                    VError "Operand Error: 01"

                        VFloat n1 ->
                            case v2 of
                                VInt n2   -> 
                                    floatOp op n1 (toFloat n2)

                                VFloat n2 -> 
                                    floatOp op n1 n2

                                _         -> 
                                    VError "Operand Error: 02"

                        VTrue ->
                            case v2 of
                                VTrue ->
                                    case op of
                                    And -> VTrue
                                    Or  -> VTrue
                                    Eq  -> VTrue 
                                    Ne  -> VFalse
                                    _   -> VError "Arithmetic Error: 02"
                                
                                VFalse ->
                                    case op of
                                        And -> VFalse
                                        Or  -> VTrue
                                        Eq  -> VFalse
                                        Ne  -> VTrue
                                        _   -> VError "Arithmetic Error: 03"
                                
                                _ ->
                                    VError "Operand Error: 03"

                        VFalse ->
                            case v2 of
                                VTrue ->
                                    case op of
                                        And -> VFalse
                                        Or  -> VTrue
                                        Eq  -> VFalse
                                        Ne  -> VTrue
                                        _   -> VError "Arithmetic Error: 04"
                                
                                VFalse ->
                                    case op of
                                        And -> VFalse
                                        Or  -> VFalse
                                        Eq  -> VTrue
                                        Ne  -> VFalse
                                        _   -> VError "Arithmetic Error: 05"
                                
                                _ ->
                                    VError "Operand Error: 04"
                        
                        VChar c1 ->
                            case v2 of
                                VChar c2 ->
                                    case op of
                                        Eq -> boolOp (c1 == c2)
                                        Ne -> boolOp (c1 /= c2)
                                        Lt -> boolOp (c1 < c2)
                                        Gt -> boolOp (c1 > c2)
                                        Le -> boolOp (c1 <= c2)
                                        Ge -> boolOp (c1 >= c2)
                                        _  -> VError "Logical Operation Error: 04"
                                _ -> VError "Operand Error: 10"

                        VCons id1 _ _ ->
                            case v2 of
                                VCons id2 _ _ -> listOp id1 id2 op v1 v2 
                                
                                VNil id2 -> listOp id1 id2 op v1 v2

                                _ ->
                                    VError "Operand Error: 08"

                        VNil id1 ->
                            case v2 of
                                VCons id2 _ _ -> listOp id1 id2 op v1 v2
                                
                                VNil id2 -> listOp id1 id2 op v1 v2

                                _ ->
                                    VError "Operand Error: 09"

                        _ ->
                            VError ("Operand Error: 05" ++ print v1)
            in
                (ECBPrim en1 en2, expr, { value = v })


        ECase _ (EVar ws s) branch ->
            case findVarByName s venv of
                Just v1 ->
                    let 
                        en1 = (ECVar, EVar ws s, { value = v1 })
                        res = matchCase v1 branch 
                    in 
                        case res.venvm of
                            [(_, VError info)] -> (ECError, expr, { value = VError info })
                            _ -> 
                                let
                                    en2 = eval (res.venvm++venv) res.ei 
                                    v2 = getValueFromExprNode en2
                                in
                                    (ECCase en1 en2, expr, { value = v2 })    
                
                Nothing ->
                    (ECError, expr, { value = VError "Variable Error: 02" })


        EFix _ e ->
            eval venv (EApp defaultWS e (EFix defaultWS e)) 


        EParens _ e ->
            eval venv e 


        ENode _ s e1 e2 ->
            let
                en1 = eval venv e1
                v1 = getValueFromExprNode en1
            
                en2 = eval venv e2
                v2 = getValueFromExprNode en2
                
                v = VNode s v1 v2
            in
                (ECNode en1 en2, expr, { value = v })
            

        EToStr _ e ->
            let
                en1 = eval venv e 
                v1 = getValueFromExprNode en1

                sv = print v1

                v = String.toList sv |> stringToVCons
            in
                (ECToStr en1, expr, { value = v })
            

        EError info ->
            let 
                v = VError info
            in
                (ECError, expr, { value = v })


        _ ->
            let
                v = VError ("Something Wrong!" ++ Debug.toString expr)
            in
                (ECError, expr, { value = v })
            


stringToVCons :  List Char -> Value
stringToVCons lc =
    case lc of
        [] ->
            VNil 1

        c :: cs ->
            VCons 1 (VChar c) (stringToVCons cs)


floatOp : Bop -> Float -> Float -> Value
floatOp op n1 n2 =
    case op of
        Add -> VFloat (n1 + n2)
        Sub -> VFloat (n1 - n2)
        Mul -> VFloat (n1 * n2)
        Div -> VFloat (n1 / n2)
        DDiv -> VFloat (n1 / n2)
        Eq -> boolOp (n1 == n2)
        Ne -> boolOp (n1 /= n2)
        Lt -> boolOp (n1 < n2)
        Gt -> boolOp (n1 > n2)
        Le -> boolOp (n1 <= n2)
        Ge -> boolOp (n1 >= n2)
        _  -> VError "Logical Operation Error: 03"




listOp : Int -> Int -> Bop -> Value -> Value -> Value
listOp id1 id2 op v1 v2 =
    case (op, id1, id2) of 
        (Cat, _, _) -> 
            if id1 == id2 then
                append v1 v2
            else 
                VError "Operand Error: 07"
        
        (Add, _, _) -> 
            if id1 == id2 then
                append v1 v2
            else 
                VError "Operand Error: 07"
        
        -- string eq
        (Eq, 1, 1) -> boolOp <| strEq v1 v2
        (Ne, 1, 1) -> boolOp <| not (strEq v1 v2)
        (Lt, 1, 1) -> boolOp <| strLt v1 v2
        (Gt, 1, 1) -> boolOp <| not (strLe v1 v2)
        (Le, 1, 1) -> boolOp <| strLe v1 v2
        (Ge, 1, 1) -> boolOp <| not (strLt v1 v2)

        _ -> VError ("Operand Error: 06" ++ Debug.toString op ++ ";"  ++ Debug.toString id1 ++ ";" ++ Debug.toString id2)

evalDictPairs : VEnv -> EDictPairs -> (ECDictPairs, VDictPairs)
evalDictPairs venv ed = 
    case ed of
        ENothing -> (ECNothing, VNothing)
        EDictPair _ name expr pairs -> 
            let
                enExpr = eval venv expr
                (ecRestPairs, vRestPairs) = evalDictPairs venv pairs
                vPairs = VDictPair name (getValueFromExprNode enExpr) vRestPairs
            in
                (ECDictPair enExpr ecRestPairs, vPairs)

