module BDEval exposing (..)

import BDModel exposing (..)
import BDSyntax exposing (..)
import BDLangUtils exposing (..)
import Round

eval :  VEnv -> Expr -> Value
eval venv expr =
    case expr of

        EVar _ s ->
            case findVarByName s venv of
                Just v ->
                    case v of
                        VFix e -> eval venv (EFix defaultWS e) 
                        val    -> val

                Nothing -> VError "Variable Error: 01"

        ELam _ p e ->
            VClosure p e venv

        ELet _ p e1 e2 ->
            eval venv (EApp defaultWS (ELam defaultWS p e2) e1)

        ELetrec _ p e1 e2 ->
            eval venv 
                (EApp defaultWS (ELam defaultWS p e2) (EFix defaultWS (ELam defaultWS p e1)))

        EApp _ e1 e2 ->
            case eval venv e1 of

                VClosure p ef venvf ->
                    case e2 of
                        EFix _ e -> 
                            case p of
                                PVar _ s -> eval ((s, VFix e)::venvf) ef 
                                _      -> VError "Recursion Error: 01"
                        _ ->  
                            let 
                                v2 =
                                    eval venv e2 
                                
                                venvm =
                                    match p v2
                            in
                            case venvm of
                                [(_, VError info)] -> VError info
                                _             -> eval (venvm++venvf) ef

                _ -> VError "Function Error: 01"

        EInt _ n ->
            VInt n

        EFloat _ n ->
            VFloat n
        
        ETrue _ -> VTrue
        EFalse _ -> VFalse

        EChar _ c -> VChar c

        ECons (_, id) e1 e2 ->
            let 
                v1 =
                    eval venv e1

                v2 =
                    eval venv e2
            in 
                if id == eoCons then
                    -- handle (::) operation on string
                    case v2 of
                        VCons vsId _ _ ->
                            case v1 of
                                VChar _ -> VCons vsId v1 v2

                                _ -> VError "Cons (::) Error: cannot cons other type except char with string"
                        
                        VNil vsId ->
                            case v1 of
                                VChar _ -> VCons vsId v1 v2

                                _ -> VError "Cons (::) Error: cannot cons other type except char with string"
                        
                        _ -> VCons voId v1 v2
                else 
                    if id == esQuo || id == esElm then 
                        VCons vsId v1 v2
                    else 
                        VCons voId v1 v2


        EBTuple _ e1 e2 ->
            let 
                v1 =
                    eval venv e1
                
                v2 =
                    eval venv e2
            in
                VBTuple v1 v2

        ETTuple _ e1 e2 e3 ->
            let 
                v1 =
                    eval venv e1
                
                v2 =
                    eval venv e2
                
                v3 =
                    eval venv e3 
            in
                VTTuple v1 v2 v3
        
        ENil (_, id) ->
            if id == esQuo || id == esElm then
                VNil 1
            else VNil 0

        EUPrim _ op e ->
            case op of
                Neg ->
                    let 
                        v =
                            eval venv e 
                    in
                        case v of
                            VInt n    ->
                                VInt (0-n)
                            VFloat n  ->
                                VFloat (0-n)
                            _         ->
                                VError "Arithmetic Error: 01"
                Not ->
                    let 
                        v =
                            eval venv e
                    in
                    case v of
                        VTrue     ->
                            VFalse
                        VFalse    ->
                            VTrue
                        _         ->
                            VError "Logical Operation Error: 01"

        EBPrim _ op e1 e2 ->
            let 
                v1 = 
                    eval venv e1

                v2 = 
                    eval venv e2
            in
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
                                _   -> VError "Arithmetic Error: 02"
                            
                            VFalse ->
                                case op of
                                    And -> VFalse
                                    Or  -> VTrue
                                    _   -> VError "Arithmetic Error: 03"
                            
                            _ ->
                                VError "Operand Error: 03"

                    VFalse ->
                        case v2 of
                            VTrue ->
                                case op of
                                    And -> VFalse
                                    Or  -> VTrue
                                    _   -> VError "Arithmetic Error: 04"
                            
                            VFalse ->
                                case op of
                                    And -> VFalse
                                    Or  -> VFalse
                                    _   -> VError "Arithmetic Error: 05"
                            
                            _ ->
                                VError "Operand Error: 04"

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
                        VError "Operand Error: 05"

        ECase _ (EVar _ s) branch ->
            case findVarByName s venv of
                Just v ->
                    case v of
                        val       ->  
                            let 
                                res = 
                                    matchCase val branch 
                            in 
                            case res.venvm of
                                [(_, VError info)] -> VError info
                                _ -> eval (res.venvm++venv) res.ei 
                
                Nothing ->
                    VError "Variable Error: 02"

        EFix _ e ->
            eval venv (EApp defaultWS e (EFix defaultWS e)) 

        EParens _ e ->
            eval venv e 

        EHtml _ s e1 e2 e3 ->
            let 
                v1 = 
                    eval venv e1 

                v2 = 
                    eval venv e2 

                v3 = 
                    eval venv e3 
            in
                VHtml s v1 v2 v3

        EToStr _ e ->
            let
                v1 =
                    eval venv e 

                sv =
                    print v1
            in
                String.toList sv |> stringToVCons
            

        EError info ->
            VError info

        _ ->
            VError "Something Wrong!"


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
        Lt -> boolOp (n1 < n2)
        Gt -> boolOp (n1 > n2)
        Le -> boolOp (n1 <= n2)
        Ge -> boolOp (n1 >= n2)
        _  -> VError "Logical Operation Error: 03"


boolOp : Bool -> Value
boolOp p =
    if p then VTrue else VFalse

listOp : Int -> Int -> Bop -> Value -> Value -> Value
listOp id1 id2 op v1 v2 =
    case op of 
        Cat -> 
            if id1 == id2 then
                append v1 v2
            else 
                VError "Operand Error: 07"
        
        Add -> 
            if id1 == id2 then
                append v1 v2
            else 
                VError "Operand Error: 07"
        
        -- list eq
        -- Eq -> 

        _ -> VError ("Operand Error: 06" ++ Debug.toString op)
