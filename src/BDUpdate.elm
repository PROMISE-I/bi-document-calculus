module BDUpdate exposing (..)

import List exposing (..)
import BDModel exposing (..)
import BDSyntax exposing (..)
import BDLangUtils exposing (..)
import BDEval exposing (eval)
import BDHtmlParser exposing (parseHtml)
import BDParser_ exposing (parse, parseVal)
import BDDesugar exposing (desugarWithPreclude)


updateCode : Model -> Code
updateCode model =
    let
        pOutput =
            case model.mode of
                HTML -> 
                    parseHtml model.output
                
                Console ->
                    let
                        resOutput =
                            parseVal model.output []
                    in
                    case resOutput of
                        Result.Ok res -> res
                        Result.Err _ -> VError "Parse Value Error."

        pCode =
            case parse model.codeBackup of
                Result.Ok res -> res
                Result.Err _ -> EError "Parse Code Error."
    in
    case (pOutput, pCode) of  
    (VError _, _) ->
        "Parse Output Error."
    (_, EError _) ->
        "Parse Code Error."
    _ -> 
        let desugaredPCode = desugarWithPreclude pCode
            expr = processAfterParse desugaredPCode []
            upRes = uneval [] expr pOutput
            expr_ = processBeforePrint upRes.expr []
            newCode = printAST expr_
        in 
            newCode


uneval : VEnv -> Expr -> Value -> UnEvalRes
uneval venv expr newv =
    case expr of
        EVar _ s ->
            case newv of
                VError info ->  
                    { venv = venv
                    , expr = EError ("Variable Update Error: " ++ info)
                    }
                
                _ ->
                    { venv = updateElmInVenv s newv venv
                    , expr = expr
                    }

        ELam ws _ _ ->
            case newv of
                VClosure p_ e_ venv_ ->
                    { venv = venv_
                    , expr = ELam ws p_ e_
                    }
                
                _ ->
                    { venv = venv
                    , expr = EError "Function Closure Update Error."
                    }

        ELet ws p e1 e2 ->
            let
                res =
                    uneval venv 
                        (EApp defaultWS (ELam defaultWS p e2) e1) newv 
            in
            case res.expr of
                EError info ->
                    { venv = []
                    , expr = EError info
                    }

                EApp _ (ELam _ p_ e2_) e1_->
                    { venv = res.venv
                    , expr = ELet ws p_ e1_ e2_
                    }

                _ ->
                    { venv = []
                    , expr = EError "ELet Update Error."
                    }

        ELetrec ws p e1 e2 ->
            let 
                res =
                    uneval venv 
                        (EApp ws (ELam defaultWS p e2) (EFix defaultWS (ELam defaultWS p e1))) newv 
            in
            case res.expr of
                EError info ->
                    { venv = []
                    , expr = EError info
                    }

                EApp _ (ELam _ p_ e2_) (EFix _ (ELam _ _ e1_))->
                    { venv = res.venv
                    , expr = ELetrec ws p_ e1_ e2_
                    }

                _ ->
                    { venv = []
                    , expr = EError "ELetrec Update Error."
                    }

        EApp ws e1 (EFix _ e2) ->
            let 
                v1 = eval venv e1
            in
            case v1 of
                VClosure p ef venvf ->
                    case p of
                        PVar _ s  ->
                            let
                                res1 =
                                    uneval ((s, VFix e2)::venvf) ef newv 
                            in
                            case res1.expr of
                                EError info ->
                                    { venv = []
                                    , expr = EError info
                                    }
                                
                                _ ->
                                    let 
                                        droplen =
                                            lengthUntil s res1.venv

                                        res1_venv =
                                            drop droplen res1.venv

                                        newv1 =
                                            VClosure p res1.expr (drop 1 res1_venv)
                                        
                                        res2 =
                                            uneval venv e1 newv1 
                                    in
                                    case res2.expr of
                                        EError info ->
                                            { venv = []
                                            , expr = EError info
                                            }
                                        
                                        _ ->
                                            case (head res1_venv) of
                                                Just (_, VClosure np ne nvenv) -> 
                                                    let
                                                        newv2 =
                                                            VClosure np ne nvenv
                                                        
                                                        res3 =
                                                            uneval venv (EFix defaultWS e2) newv2 
                                                    in
                                                        { venv = venv
                                                        , expr = EApp ws res2.expr res3.expr
                                                        }

                                                Just (_, VFix e21) ->
                                                        { venv = venv
                                                        , expr = EApp ws res2.expr (EFix defaultWS e21)
                                                        }
                                                
                                                Just (_, VError info) ->
                                                    { venv = []
                                                    , expr = EError info
                                                    }
                                                
                                                _       ->
                                                    { venv = []
                                                    , expr = EError "Recursion Update Error: 01"
                                                    }
                        
                        _ ->
                            { venv = venv
                            , expr = EError "Recursion Update Error: 02"
                            }

                _ ->
                    { venv = venv
                    , expr = EError "Recursion Update Error: 03"
                    }

        EApp ws e1 e2 ->
            let 
                v1 = eval venv e1
            in
            case v1 of
                VClosure p ef venvf ->
                    let 
                        v2 = eval venv e2 
                        
                        venvm = match p v2

                        res1 = uneval (venvm++venvf) ef newv
                    in
                    case res1.expr of
                        EError info ->
                            { venv = []
                            , expr = EError info
                            }
                        
                        _ ->
                            let 
                                newv1 =
                                    VClosure p res1.expr (drop (length venvm) res1.venv)
                                
                                res2 =
                                    uneval venv e1 newv1
                            in
                            case res2.expr of
                                EError info ->
                                    { venv = []
                                    , expr = EError info
                                    }
                                
                                _ ->
                                    let
                                        newv2 =   
                                            patternSubst res1.venv p
                                        
                                        res3 =
                                            uneval venv e2 newv2 
                                    in
                                    case res3.expr of
                                        EError info ->
                                            { venv = []
                                            , expr = EError info
                                            } 
                                        
                                        _ ->
                                            let
                                                newvenv =
                                                    mergeVEnv res2.venv res3.venv venv

                                            in
                                                { venv = newvenv
                                                , expr = EApp ws res2.expr res3.expr
                                                }
                                        
                _ ->
                    { venv = []
                    , expr = EError "Application Update Error."
                    }

        EInt ws _ ->
            case newv of
                VInt n_ ->
                    { venv = venv
                    , expr = EInt ws n_
                    }
                
                _ ->
                    { venv = []
                    , expr = EError "Int Constant Update Error."
                    }

        EFloat ws _ ->
            case newv of
                VInt n_ ->
                    { venv = venv
                    , expr = EFloat ws (toFloat n_)
                    }

                VFloat n_ ->
                    { venv = venv
                    , expr = EFloat ws n_
                    }
                
                _ ->
                    { venv = []
                    , expr = EError "Float Constant Update Error."
                    }

        ETrue ws ->
            case newv of
                VTrue ->
                    { venv = venv
                    , expr = ETrue ws
                    }

                VFalse ->
                    { venv = venv
                    , expr = EFalse ws
                    }
                
                _ ->
                    { venv = []
                    , expr = EError "True Constant Update Error."
                    }

        EFalse ws ->
            case newv of
                VTrue ->
                    { venv = venv
                    , expr = ETrue ws
                    }

                VFalse ->
                    { venv = venv
                    , expr = EFalse ws
                    }

                _ ->
                    {venv = []
                    , expr = EError "False Constant Update Error."
                    }

        EChar ws _ ->
            case newv of
                VChar c_ ->
                    { venv = venv
                    , expr = EChar ws c_
                    }

                _ ->
                    { venv = []
                    , expr = EError "Char Constant Update Error."
                    }
        

        ECons (pads, eId) e1 e2 ->
            case newv of
                VCons vId v1 v2 ->
                    let
                        res1 =
                            uneval venv e1 v1

                        res2 = 
                            uneval venv e2 v2
                        
                        newvenv =
                            mergeVEnv res1.venv res2.venv venv
                        
                    in
                        if eId == eoCons then
                            { venv = newvenv
                            , expr = ECons (pads, eId) res1.expr res2.expr
                            }
                        else 
                            { venv = newvenv
                            , expr = ECons (pads, vIdToEId vId eId) res1.expr res2.expr
                            }

                VNil vId ->
                    { venv = venv
                    , expr = ENil (pads, vIdToEId vId eId)
                    }

                _ ->
                    { venv = []
                    , expr = EError "List Update Error."
                    }

        EBTuple ws e1 e2 ->
            case newv of
                VBTuple v1 v2 ->
                    let
                        res1 =
                            uneval venv e1 v1

                        res2 = 
                            uneval venv e2 v2
                        
                        newvenv =
                            mergeVEnv res1.venv res2.venv venv

                    in
                        { venv = newvenv
                        , expr = EBTuple ws res1.expr res2.expr
                        }

                _ ->
                    { venv = []
                    , expr = EError "Tuple2 Update Error."
                    }
        
        ETTuple ws e1 e2 e3 ->
            case newv of
                VTTuple v1 v2 v3 ->
                    let
                        res1 =
                            uneval venv e1 v1

                        res2 = 
                            uneval venv e2 v2

                        res3 = 
                            uneval venv e3 v3
                        
                        newvenv =
                            mergeVEnv4 res1.venv res2.venv res3.venv venv

                    in
                        { venv = newvenv
                        , expr = ETTuple ws res1.expr res2.expr res3.expr
                        }

                _ ->
                    { venv = []
                    , expr = EError "Tuple3 Update Error."
                    }

        EHtml ws s e1 e2 e3 ->
            case newv of
                VHtml _ v1 v2 v3 ->
                    let
                        res1 =
                            uneval venv e1 v1

                        res2 = 
                            uneval venv e2 v2

                        res3 = 
                            uneval venv e3 v3
                        
                        newvenv =
                            mergeVEnv4 res1.venv res2.venv res3.venv venv

                    in
                        { venv = newvenv
                        , expr = EHtml ws s res1.expr res2.expr res3.expr
                        }

                _ -> let _ = newv in
                    { venv = []
                    , expr = EError "HTML Update Error."
                    }

        ENil (pads, eId) ->
            case newv of
                VNil vId ->
                    { venv = venv
                    , expr = ENil (pads, vIdToEId vId eId)
                    }
                
                VCons vId _ _ ->
                    let
                        ne =
                            valueToExpr newv
                        
                        newe =
                            case ne of
                                ECons _ e1 e2 ->
                                    -- Original ENil is at the head of the list/string, thus need a esQuo/eoSquare
                                    if eId == esQuo || eId == eoSquare || eId == eoAddFromEmp then
                                        if vId == vsId then
                                            changeWsForList ([" "], esElm) e2
                                                    |> ECons (pads, esQuo) e1
                                    
                                        else if vId == voId then
                                            changeWsForList ([" "], eoElm) e2
                                                |> ECons (pads, eoSquare) e1
                                    
                                        else 
                                            EError "Nil Expr WS Error."
                                    -- Original ENil is one of the elements in the list/string
                                    else if eId == esElm || eId == eoElm then
                                        if vId == vsId then
                                            changeWsForList ([], esElm) ne
                                        
                                        else if vId == voId then
                                            changeWsForList ([], eoElm) ne
                                        
                                        else 
                                            EError "Nil Expr WS Error."
                                    
                                    else 
                                        EError "Nil Expr WS Error."
                                
                                _ ->
                                    EError "Value To Expr Error."
                    in                            
                        { venv = venv
                        , expr = newe
                        }

                _ ->
                    { venv = []
                    , expr = EError "Nil List Update Error."
                    }

        EFix ws e ->
            let
                res = uneval venv (EApp defaultWS e (EFix defaultWS e)) newv

                e_ =
                    case res.expr of
                        EApp _ e1 (EFix _ e2) ->
                            if e2 /= e then e2 else e1
                        _ ->
                            EError "Fix Update Error."
            in
                { venv = res.venv
                , expr = EFix ws e_
                }

        ECase ws1 (EVar ws2 s) branches ->
            let
                res = findVarByName s venv 
            in
            case (res, newv) of
                (Just v, _) ->
                    let
                        tryRes = 
                            case v of
                                _  ->
                                    let
                                        matchRes =
                                            matchCase v branches

                                        resi =
                                            uneval (matchRes.venvm++venv) matchRes.ei newv
                                    in
                                    { ei     = resi.expr
                                    , venv   = resi.venv
                                    , choice = matchRes.choice
                                    , pi     = matchRes.pi
                                    }
                    in
                    case tryRes.ei of
                        EError info ->
                            { venv = []
                            , expr = EError info
                            }

                        _ ->   
                            let 
                                branches_ =
                                    updateBranch branches tryRes.choice tryRes.ei
                                
                                len =
                                    length tryRes.venv - length venv

                                newv_ = tryRes.pi 
                                        |> patternSubst tryRes.venv 

                                venv_ =
                                    ((s, newv_) :: (drop (len + 1) tryRes.venv))
                            in
                                { venv = venv_
                                , expr = ECase ws1 (EVar ws2 s) branches_
                                }
                
                _ ->
                    { venv = []
                    , expr = EError "Case Expression Error."
                    }

        EUPrim ws op e ->
            let 
                v = eval venv e
            in
            case op of
                Not ->
                    case v of
                        VTrue ->
                            case newv of
                                VTrue ->
                                    let 
                                        res =
                                            uneval venv e VFalse
                                    in
                                        { venv = res.venv
                                        , expr = EUPrim ws Not res.expr
                                        }

                                VFalse ->
                                    { venv = venv
                                    , expr = EUPrim ws Not e
                                    }

                                _ -> 
                                    { venv = []
                                    , expr = EError "Unary Expression Error: 02"
                                    }

                        VFalse ->
                            case newv of
                                VTrue ->
                                    { venv = venv
                                    , expr = EUPrim ws Not e
                                    }

                                VFalse ->
                                    let 
                                        res = uneval venv e VTrue
                                    in
                                        { venv = res.venv
                                        , expr = EUPrim ws Not res.expr
                                        }

                                _ -> 
                                    { venv = []
                                    , expr = EError "Unary Expression Error: 03"
                                    }
                        
                        _ ->
                            { venv = []
                            , expr = EError "Unary Expression Error: 04"
                            }

                Neg ->
                    case v of
                        VInt n ->
                            case newv of
                                VInt n_ ->
                                    if n == (-n_) then 
                                        { venv = venv
                                        , expr = EUPrim ws Neg e
                                        }
                                    else
                                        let
                                            res = uneval venv e (VInt (-n_))
                                        in
                                            { venv = res.venv
                                            , expr = EUPrim ws Neg res.expr
                                            }

                                _ -> 
                                    { venv = []
                                    , expr = EError "Unary Expression Error: 06"
                                    }

                        VFloat n ->
                            case newv of
                                VInt n_ ->
                                    if n == toFloat (-n_) then 
                                        { venv = venv
                                        , expr = EUPrim ws Neg e
                                        }
                                    else
                                        let 
                                            res = uneval venv e (VFloat (toFloat (-n_)))
                                        in
                                            { venv = res.venv
                                            , expr = EUPrim ws Neg res.expr
                                            }

                                VFloat n_ ->
                                    if n == (-n_) then 
                                        { venv = venv
                                        , expr = EUPrim ws Neg e
                                        }
                                    else
                                        let 
                                            res = uneval venv e (VFloat (-n_))
                                        in
                                            { venv = res.venv
                                            , expr = EUPrim ws Neg res.expr
                                            }

                                _ -> 
                                    { venv = []
                                    , expr = EError "Unary Expression Error: 07"
                                    }

                        
                        _ ->
                            { venv = []
                            , expr = EError "Unary Expression Error: 08"
                            }

        EBPrim ws op e1 e2 ->
            let
                logic_ =
                    logic ws e1 e2 venv newv

                arith_ = 
                    arith ws e1 e2 venv newv

                comp_ = 
                    comp ws e1 e2 venv newv
            in
            case op of
                And -> logic_ And
                Or  -> logic_ Or
                
                Add -> arith_ Add
                Sub -> arith_ Sub
                Mul -> arith_ Mul
                Div -> arith_ Div
                DDiv -> arith_ DDiv
                Cat -> arith_ Cat

                _ -> comp_ op

        EParens ws e ->
            let 
                res = uneval venv e newv
            in
                { venv = res.venv
                , expr = EParens ws res.expr
                }

        EToStr ws e ->
            case newv of
                VCons 1 _ _ ->
                    let
                        res1 = parseVal (vconsToString newv) []
                    in
                    case res1 of
                        Result.Ok nv ->
                            let
                                res2 = uneval venv e nv
                            in
                                { venv = res2.venv
                                , expr = EToStr ws res2.expr
                                }
                        
                        Result.Err _ ->
                            { venv = []
                            , expr = EError "Cannot update toString because of wrong newv."
                            }

                _ ->
                    { venv = []
                    , expr = EError "toString Update Error."
                    }

        _ ->
            { venv = []
            , expr = EError "Source Expression Error."
            }


vconsToString : Value -> String
vconsToString v =
    case v of
        VNil 1 ->
            ""
        
        VCons 1 (VChar c) v2 ->
            (String.fromChar c) ++ (vconsToString v2)

        _ ->
            "VCons To String Error."


logic : WS -> Expr -> Expr -> VEnv -> Value -> Bop -> UnEvalRes
logic ws e1 e2 venv newv op =
    let
        v1 = eval venv e1
        
        v2 = eval venv e2

        v = eval venv (EBPrim ws op e1 e2)
        
        (newv1, newv2) =
            case v of
                VTrue ->
                    case newv of
                        VTrue -> (v1, v2)

                        VFalse ->
                            case (v1, v2, op) of
                                (VTrue, VTrue, And) ->
                                    (VFalse, VTrue)
                                
                                (VTrue, VFalse, Or) ->
                                    (VFalse, VFalse)

                                (VFalse, VTrue, Or) ->
                                    (VFalse, VFalse)

                                _ ->
                                    (VError "", VError "")
                        
                        _ ->
                            (VError "", VError "")
                
                VFalse ->
                    case newv of
                        VFalse -> (v1, v2)

                        VTrue ->
                            case (v1, v2, op) of
                                (VFalse, VFalse, Or) ->
                                    (VTrue, VFalse)

                                (VTrue, VFalse, And) ->
                                    (VTrue, VTrue)

                                (VFalse, VTrue, And) ->
                                    (VTrue, VTrue)

                                _ ->
                                    (VError "", VError "")
                        
                        _ ->
                            (VError "", VError "")

                _ ->
                    (VError "", VError "")
    in
    case (newv1, newv2) of
        (VError _, VError _) ->
            { venv = []
            , expr = EError "Missing Information or Logic Expression Error."
            }

        _ ->
            checkChange venv ws op e1 e2 v1 v2 newv1 newv2


arith : WS -> Expr -> Expr -> VEnv -> Value -> Bop -> UnEvalRes
arith ws e1 e2 venv newv op =
    let
        v1 = eval venv e1 
        
        v2  = eval venv e2
    in
    case newv of
        VInt n ->
            let
                (newv1, newv2) =
                    case (v1, v2) of
                        (VFloat n1, _) ->
                            case op of
                                Add -> (VFloat n1, VFloat ((toFloat n) - n1))
                                Sub -> (VFloat n1, VFloat (n1 - (toFloat n)))
                                Mul -> (VFloat n1, VFloat ((toFloat n) / n1))
                                Div -> (VFloat n1, VFloat (n1 / (toFloat n)))
                                DDiv -> (VFloat n1, VFloat (n1 / (toFloat n)))
                                _   -> (VError "", VError "")
                        
                        (_, VFloat n2) ->
                            case op of
                                Add -> (VFloat ((toFloat n) - n2), VFloat n2)
                                Sub -> (VFloat ((toFloat n) + n2), VFloat n2)
                                Mul -> (VFloat ((toFloat n) / n2), VFloat n2)
                                Div -> (VFloat ((toFloat n) * n2), VFloat n2)
                                DDiv -> (VFloat ((toFloat n) * n2), VFloat n2)
                                _   -> (VError "", VError "")
                        
                        (VInt n1, VInt n2) ->
                            case op of
                                Add -> (VInt n1, VInt (n - n1))
                                Sub -> (VInt n1, VInt (n1 - n))
                                Mul ->
                                    if (n1 * n2) == n then
                                        (VInt n1, VInt n2)
                                    else
                                        (VInt n1, VInt (n // n1))
                                Div ->
                                    if (n1 // n2) == n then
                                        (VInt n1, VInt n2)
                                    else
                                        (VInt n1, VInt (n1 // n))
                                DDiv ->
                                    if (n1 // n2) == n then
                                        (VInt n1, VInt n2)
                                    else
                                        (VInt n1, VInt (n1 // n))
                                _   -> (VError "", VError "")
                        
                        _ -> (VError "", VError "")
            in
            case (newv1, newv2) of
                (VError _, VError _) ->
                    { venv = []
                    , expr = EError "Missing Information or Operands Type Error: 01."
                    }
                
                _ ->
                    checkChange venv ws op e1 e2 v1 v2 newv1 newv2

        VFloat n ->
            let
                (newv1, newv2) =
                    case (v1, v2) of
                        (VFloat n1, _) ->
                            case op of
                                Add -> (VFloat n1, VFloat (n - n1))
                                Sub -> (VFloat n1, VFloat (n1 - n))
                                Mul -> (VFloat n1, VFloat (n / n1))
                                Div -> (VFloat n1, VFloat (n1 / n))
                                DDiv -> (VFloat n1, VFloat (n1 / n))
                                _   -> (VError "", VError "")
                        
                        (_, VFloat n2) ->
                            case op of
                                Add -> (VFloat (n - n2), VFloat n2)
                                Sub -> (VFloat (n + n2), VFloat n2)
                                Mul -> (VFloat (n / n2), VFloat n2)
                                Div -> (VFloat (n * n2), VFloat n2)
                                DDiv -> (VFloat (n * n2), VFloat n2)
                                _   -> (VError "", VError "")

                        (VInt n1, VInt _) ->
                            case op of
                                DDiv -> (VInt n1, VInt (Basics.round ((toFloat n1) / n)))
                                _   -> (VError "", VError "")
                        
                        _ -> (VError "", VError "")
            in
            case (newv1, newv2) of
                (VError _, VError _) ->
                    { venv = []
                    , expr = EError "Missing Informatin or Operands Type Error: 02."
                    }
                
                _ -> 
                    checkChange venv ws op e1 e2 v1 v2 newv1 newv2

        VCons id _ _ ->
            case op of
                Cat ->
                    let
                        (newv1, newv2) = deAppend newv (vlength v1)
                    in
                        checkChange venv ws op e1 e2 v1 v2 newv1 newv2

                Add -> 
                    if id == vsId then 
                        let 
                            (newv1, newv2) = deAppend newv (vlength v1)
                        in 
                            checkChange venv ws op e1 e2 v1 v2 newv1 newv2
                    else 
                        { venv = []
                        , expr = EError "Arith Expression Modified Value Type Error: 01."
                        }

                _ -> 
                    { venv = []
                    , expr = EError "Arith Expression Modified Value Type Error: 01."
                    }

        VNil _ ->
            if op == Cat then
                checkChange venv ws op e1 e2 v1 v2 newv newv
            else
                { venv = []
                , expr = EError "Arith Expression Modified Value Type Error: 02."
                }

        _ -> 
            { venv = []
            , expr = EError "Arith Expression Modified Value Type Error: 03."
            }


comp : WS -> Expr -> Expr -> VEnv -> Value -> Bop -> UnEvalRes
comp ws e1 e2 venv newv op =
    let
        v1= eval venv e1
        
        v2 = eval venv e2
    in
    case newv of
        VTrue ->
            case op of
                Eq ->
                    let 
                        (newv1, newv2) =
                            case (v1, v2) of
                                _ -> (VError "", VError "")
                    in
                    case (newv1, newv2) of
                        (VError _, VError _) ->
                            { venv = []
                            , expr = EError "Missing Information, Cannot Infer: 01."
                            }
                        _ ->
                            checkChange venv ws op e1 e2 v1 v2 newv1 newv2

                _ ->
                    let
                        res = eval venv (EBPrim ws op e1 e2)
                        
                        newe =
                            case res of
                                VTrue -> EBPrim ws op e1 e2
                                VFalse ->
                                    case op of
                                        Lt -> EBPrim ws Ge e1 e2
                                        Gt -> EBPrim ws Le e1 e2
                                        Le -> EBPrim ws Gt e1 e2
                                        Ge -> EBPrim ws Lt e1 e2
                                        _  ->
                                            EError "Comparison Expression Modified Type Error: 01."
                                _ ->
                                    EError "Missing Information, Cannot Infer: 02."
                    in
                        { venv = venv
                        , expr = newe
                        } 

        VFalse ->
            let
                res = eval venv (EBPrim ws op e1 e2)
                
                newe =
                    case res of
                        VFalse -> EBPrim ws op e1 e2
                        VTrue ->
                            case op of
                                Lt -> EBPrim ws Ge e1 e2
                                Gt -> EBPrim ws Le e1 e2
                                Le -> EBPrim ws Gt e1 e2
                                Ge -> EBPrim ws Lt e1 e2
                                Eq -> EError "Missing Information, Cannot Infer: 03."
                                _  -> EError "Comparison Expression Modified Type Error: 02."
                        _ ->
                            EError "Missing Information, Cannot Infer: 04."
            in
                { venv = venv
                , expr = newe
                } 

        _ ->
            { venv = []
            , expr = EError "Comparison Expression Modified Type Error: 03."
            }


checkChange : VEnv -> WS -> Bop -> 
            Expr -> Expr -> Value -> Value -> Value -> Value -> UnEvalRes
checkChange venv ws op e1 e2 v1 v2 newv1 newv2 =
    if newv1 == v1 && newv2 == v2 then
        { venv = venv
        , expr = EBPrim ws op e1 e2
        }
    else if newv1 == v1 then
        let
            res2 = uneval venv e2 newv2
        in
            { venv = res2.venv
            , expr = EBPrim ws op e1 res2.expr
            }
    else if newv2 == v2 then
        let
            res1 = uneval venv e1 newv1
        in
            { venv = res1.venv
            , expr = EBPrim ws op res1.expr e2
            }
    else
        let
            res1 = uneval venv e1 newv1

            res2 = uneval venv e2 newv2

            newvenv =
                mergeVEnv res1.venv res2.venv venv

        in
            { venv = newvenv
            , expr = EBPrim ws op res1.expr res2.expr
            }