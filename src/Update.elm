module Update exposing (..)

import List exposing (..)
import Model exposing (..)
import Syntax exposing (..)
import LangUtils exposing (..)
import Eval exposing (eval)
import HtmlParser exposing (parseHtml)
import Parser_ exposing (parse, parseVal)
import Desugar exposing (..)
import Resugar exposing (..)


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

            exprNode = eval [] expr
            oldv = getValueFromExprNode exprNode
            newv = pOutput
            diffs = calcDiff oldv newv
            upRes = uneval [] expr newv diffs

            expr_ = processBeforePrint upRes.expr []
            resugaredCode = resugarWithoutPreclude expr_

            newCode = printAST resugaredCode

            -- _ = Debug.log "pCode" <| Debug.toString pCode
            -- _ = Debug.log "pOutput" <| Debug.toString pOutput
            -- _ = Debug.log "upRes.expr" <| Debug.toString upRes.expr
            -- _ = Debug.log "expr_" <| Debug.toString expr_
            -- _ = Debug.log "resugaredCode" <| Debug.toString resugaredCode
            -- _ = Debug.log "diffs" <| Debug.toString diffs

        in 
            newCode


uneval : VEnv -> Expr -> Value -> List (DiffOp Value) -> UnEvalRes
uneval venv expr newv diffs =
    -- let
    --     expr = getExprFromExprNode exprNode
    -- in
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
                    , expr = EError ("Function Closure Update Error." ++ (Debug.toString newv))
                    }

        ELet ws p e1 e2 ->
            let
                res =
                    uneval venv 
                        (EApp defaultWS (ELam defaultWS p e2) e1) newv diffs
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
                        (EApp ws (ELam defaultWS p e2) (EFix defaultWS (ELam defaultWS p e1))) newv diffs
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


    -- ad hoc uneval semantics of $map$
        EApp
            ws1
            (
                EApp 
                    ws2
                    (EVar ws3 "map")
                    e1
            )
            e2 ->
                let
                    (newvenv, newe1, newe2) = mapUneval venv e1 e2 newv diffs
                in
                    { venv = newvenv
                    , expr = EApp ws1 (EApp ws2 eVarMap newe1) newe2
                    }              
                
        -- ad hoc uneval semantics of $flatten$
        -- EApp
        --     ws1 
        --     (EVar ws2 "flatten")
        --     e ->
        --         let
        --             (newvenv, newe) = flattenUneval venv e newv diffs
        --         in
        --             { venv = newvenv
        --             , expr = EApp ws1 eVarFlatten newe 
        --             }

        -- ad hoc uneval semantics of $append$
        EApp
            ws1
            (
                EApp 
                    ws2
                    (EVar ws3 "append")
                    e1
            )
            e2 ->
            let
                en1 = eval venv e1
                v1 = getValueFromExprNode en1
                en2 = eval venv e2
                v2 = getValueFromExprNode en2

                (v1Diffs, v2Diffs) = splitDiffs (vConsToList v1) (vConsToList v2) diffs
                newv1 = applyDiffs v1 v1Diffs
                newv2 = applyDiffs v2 v2Diffs

                e1NewRes = uneval venv e1 newv1 v1Diffs
                e2NewRes = uneval venv e2 newv2 v2Diffs
                venvNew = mergeVEnv e1NewRes.venv e2NewRes.venv venv

                -- _ = Debug.log "e1" <| Debug.toString e1
                -- _ = Debug.log "e2" <| Debug.toString e2
                -- _ = Debug.log "v1" <| Debug.toString v1
                -- _ = Debug.log "v2" <| Debug.toString v2
                -- _ = Debug.log "newv1" <| Debug.toString newv1
                -- _ = Debug.log "newv2" <| Debug.toString newv2
                -- _ = Debug.log "v1Diffs" <| Debug.toString v1Diffs
                -- _ = Debug.log "v2Diffs" <| Debug.toString v2Diffs
                -- _ = Debug.log "e1New" <| Debug.toString e1NewRes.expr
                -- _ = Debug.log "e2New" <| Debug.toString e2NewRes.expr


            in 
                { venv = venvNew
                , expr = (EApp ws1 (EApp ws2 eVarAppend e1NewRes.expr) e2NewRes.expr)
                }


        EApp ws e1 (EFix _ e2) ->
            let 
                en1 = eval venv e1
                v1 = getValueFromExprNode en1
            in
            case v1 of
                VClosure p ef venvf ->
                    case p of
                        PVar _ s  ->
                            let
                                res1 = uneval ((s, VFix e2)::venvf) ef newv diffs
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

                                        v1Diffs = calcDiff v1 newv1
                                        
                                        res2 = uneval venv e1 newv1 v1Diffs
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
                                                        newv2 = VClosure np ne nvenv
                                                        
                                                        res3 = uneval venv (EFix defaultWS e2) newv2 []
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
            if e1 == lamTplStr || e1 == lamTplNode || e1 == lamTplExpr then
                case e2 of 
                    ECons ws1 tPartE restTplExpr ->
                        case diffs of
                            [] -> 
                                { venv = []
                                , expr = EError "Lambda Template Part Update Error: diffs is empty!"}

                            (DiffDelete _) :: restDiffs -> uneval venv restTplExpr newv restDiffs

                            (DiffInsert vTPart) :: restDiffs ->
                                let
                                    newTPartE = valueToExpr vTPart |> addQuoOrSquareForList
                                    
                                    res2 = uneval venv expr (vConsTail newv) restDiffs
                                in
                                    { venv = res2.venv
                                    , expr = makeTplIdentity e1 ws tPartE newTPartE res2.expr  
                                    }  

                            (DiffKeep _) :: restDiffs ->
                                let
                                    res2 = uneval venv restTplExpr (vConsTail newv) restDiffs
                                in
                                    { venv = res2.venv 
                                    , expr = EApp ws e1 (ECons ws1 tPartE res2.expr)
                                    }
                            
                            (DiffUpdate newvTPart) :: restDiffs ->
                                let
                                    enTPart = eval venv tPartE
                                    vTPart = getValueFromExprNode enTPart
                                    vTPartDiffs = calcDiff vTPart newvTPart

                                    res1 = uneval venv tPartE newvTPart vTPartDiffs
                                    res2 = uneval venv restTplExpr (vConsTail newv) restDiffs 

                                    newvenv = mergeVEnv res1.venv res2.venv venv

                                    tpWs = alignTpWs (getVNodeName vTPart) (getVNodeName newvTPart) ws

                                    -- _ = Debug.log "env-update-v" <| Debug.toString [vTPart, newvTPart]
                                    -- _ = Debug.log "env-update-e" <| Debug.toString res1.expr
                                    -- _ = Debug.log "env-update-ws" <| Debug.toString [ws, tpWs]
                                            
                                in
                                    { venv = newvenv
                                    , expr = EApp tpWs e1 (ECons ws1 res1.expr res2.expr)}  

                    _ -> 
                        { venv = []
                        , expr = EError "Invalid template part (tplStr or tplNode or tplExpr) expression!"}      
                    

            else
                let 
                    en1 = eval venv e1
                    v1 = getValueFromExprNode en1
                in
                case v1 of
                    VClosure p ef venvf ->
                        let 
                            en2 = eval venv e2 
                            v2 = getValueFromExprNode en2
                            
                            venvm = match p v2

                            res1 = uneval (venvm++venvf) ef newv diffs
                        in
                        case res1.expr of
                            EError info ->
                                { venv = []
                                , expr = EError info
                                }
                            
                            _ ->
                                let 
                                    newv1 = VClosure p res1.expr (drop (length venvm) res1.venv)
                                    v1Diffs = calcDiff v1 newv1

                                    res2 =
                                        uneval venv e1 newv1 v1Diffs
                                in
                                case res2.expr of
                                    EError info ->
                                        { venv = []
                                        , expr = EError info
                                        }
                                    
                                    _ ->
                                        let
                                            newv2 = patternSubst res1.venv p
                                            v2Diffs = calcDiff v2 newv2
                                            
                                            res3 = uneval venv e2 newv2 v2Diffs

                                            -- _ = Debug.log "uneval-patternsubst-e1" <| Debug.toString e1
                                            -- _ = Debug.log "uneval-patternsubst-e2" <| Debug.toString e2
                                            -- _ = Debug.log "uneval-patternsubst-ef" <| Debug.toString ef
                                            -- _ = Debug.log "uneval-patternsubst-newv" <| Debug.toString newv
                                            -- _ = Debug.log "uneval-patternsubst-res1" <| Debug.toString res1
                                            -- _ = Debug.log "uneval-patternsubst-venvm" <| Debug.toString venvm


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
        

        ECons ((_, eid) as ws) e1 e2 -> -- TODO: change update rules to Sketch-n-Sketch
            case diffs of
                [] -> 
                    { venv = []
                    , expr = EError ("List Update Error." ++ (Debug.toString newv)) 
                    }

                (DiffDelete _) :: restDiffs ->
                    let
                        res2 = uneval venv e2 newv restDiffs
                    in
                        { venv = res2.venv
                        , expr = changeWs ws res2.expr
                        }

                (DiffInsert newv1) :: restDiffs ->
                    case newv of
                        VCons _ _ newv2 -> 
                            let
                                newe1 = valueToExpr newv1 |> addQuoOrSquareForList
                                res2 = uneval venv expr newv2 restDiffs

                                tWS = 
                                    if eid == eoSquare then
                                        ([" "], eoElm)
                                    else if eid == esQuo then
                                        ([], esElm)
                                    else 
                                        ws 
                                t = changeWs tWS res2.expr
                            in
                                { venv = res2.venv
                                , expr = ECons ws newe1 t}

                        _ ->
                            { venv = []
                            , expr = EError ("Cons Update Error: 02" ++ " - " ++ Debug.toString newv ++ " - " ++ Debug.toString expr)}

                (DiffKeep _) :: restDiffs ->
                    case newv of
                        VCons _ _ newv2 ->
                            let
                                res2 = uneval venv e2 newv2 restDiffs
                                t = res2.expr
                            in
                                { venv = res2.venv
                                , expr = ECons ws e1 t
                                }

                        _ -> 
                            { venv = []
                            , expr = EError ("Cons Update Error: 01" ++ " - " ++ Debug.toString newv ++ " - " ++ Debug.toString expr)}
                    
                (DiffUpdate newv1) :: restDiffs ->
                    case newv of
                        VCons _ _ newv2 ->
                            let
                                en1 = eval venv e1
                                v1 = getValueFromExprNode en1
                                v1Diffs = calcDiff v1 newv1

                                res1 = uneval venv e1 newv1 v1Diffs
                                res2 = uneval venv e2 newv2 restDiffs
                                
                                newvenv = mergeVEnv res1.venv res2.venv venv
                                h = res1.expr
                                t = res2.expr

                                -- _ = Debug.log "v1-----" <| Debug.toString v1
                                -- _ = Debug.log "v1Diffs" <| Debug.toString v1Diffs
                                -- _ = Debug.log "newe1" <| Debug.toString h
                                -- _ = Debug.log "newv1" <| Debug.toString newv1

                            in
                                { venv = newvenv
                                , expr = ECons ws h t
                                }

                        _ -> 
                            { venv = []
                            , expr = EError "Cons Update Error: 03"}


        EBTuple ws e1 e2 ->
            let
                en1 = eval venv e1 
                v1 = getValueFromExprNode en1
                
                en2 = eval venv e2
                v2 = getValueFromExprNode en2
            in
            
            case newv of
                VBTuple newv1 newv2 ->
                    let
                        v1Diffs = calcDiff v1 newv1
                        v2Diffs = calcDiff v2 newv2

                        res1 = uneval venv e1 newv1 v1Diffs
                        res2 = uneval venv e2 newv2 v2Diffs
                        
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
            let
                en1 = eval venv e1 
                v1 = getValueFromExprNode en1
                
                en2 = eval venv e2
                v2 = getValueFromExprNode en2

                en3 = eval venv e3
                v3 = getValueFromExprNode en3
            in
            
            case newv of
                VTTuple newv1 newv2 newv3 ->
                    let
                        v1Diffs = calcDiff v1 newv1
                        v2Diffs = calcDiff v2 newv2
                        v3Diffs = calcDiff v3 newv3

                        res1 = uneval venv e1 newv1 v1Diffs
                        res2 = uneval venv e2 newv2 v2Diffs
                        res3 = uneval venv e3 newv3 v3Diffs
                        
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

        ENode ws s e1 e2 ->
            let
                en1 = eval venv e1 
                v1 = getValueFromExprNode en1
                
                en2 = eval venv e2
                v2 = getValueFromExprNode en2
            in
            

            case newv of
                VNode _ newv1 newv2 ->
                    let
                        v1Diffs = calcDiff v1 newv1
                        v2Diffs = calcDiff v2 newv2

                        res1 = uneval venv e1 newv1 v1Diffs
                        res2 = uneval venv e2 newv2 v2Diffs

                        newvenv =
                            mergeVEnv res1.venv res2.venv venv

                        -- _ = Debug.log "v1Diffs" <| v1Diffs
                        -- _ = Debug.log "v2Diffs" <| v2Diffs
                        -- _ = Debug.log "v1" <| v1
                        -- _ = Debug.log "v2" <| v2
                        -- _ = Debug.log "e1" <| e1
                        -- _ = Debug.log "e2" <| e2
                        -- _ = Debug.log "newv1" <| newv1
                        -- _ = Debug.log "newv2" <| newv2
                        -- _ = Debug.log "newe1" <| res1.expr
                        -- _ = Debug.log "newe2" <| res2.expr

                    in
                        { venv = newvenv
                        , expr = ENode ws s res1.expr res2.expr
                        }

                _ -> let _ = newv in
                    { venv = []
                    , expr = EError "Node Update Error."
                    }

        ENil (pads, eId) ->
            case newv of
                -- TODO: separate list nil from string nil
                VNil vId ->
                    { venv = venv
                    , expr = ENil (pads, vIdToEId vId eId)
                    }
                
                VCons vId _ _ ->
                    let
                        ne = valueToExpr newv
                        
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
                    , expr = EError ("Nil List Update Error.\n" ++ (Debug.toString newv))
                    }

        EFix ws e ->
            let
                res = uneval venv (EApp defaultWS e (EFix defaultWS e)) newv diffs

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
                                            uneval (matchRes.venvm++venv) matchRes.ei newv diffs
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

                                venv_ = updateElmInVenv s newv_ (drop len tryRes.venv)
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
                en = eval venv e
                v = getValueFromExprNode en
            in
            case op of
                Not ->
                    case v of
                        VTrue ->
                            case newv of
                                VTrue ->
                                    let 
                                        res = uneval venv e VFalse []
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
                                        res = uneval venv e VTrue []
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
                                            res = uneval venv e (VInt (-n_)) []
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
                                            res = uneval venv e (VFloat (toFloat (-n_))) []
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
                                            res = uneval venv e (VFloat (-n_)) []
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
                And -> logic_ And diffs
                Or  -> logic_ Or diffs
                
                Add -> arith_ Add diffs
                Sub -> arith_ Sub diffs
                Mul -> arith_ Mul diffs
                Div -> arith_ Div diffs
                DDiv -> arith_ DDiv diffs
                Cat -> arith_ Cat diffs

                _ -> comp_ op diffs

        EParens ws e ->
            let 
                res = uneval venv e newv diffs
            in
                { venv = res.venv
                , expr = EParens ws res.expr
                }

        EToStr ws e ->
            let
                en = eval venv e
                v = getValueFromExprNode en
            in
            case newv of
                VCons 1 _ _ ->
                    let
                        res1 = parseVal (vconsToString newv) []
                    in
                    case res1 of
                        Result.Ok nv ->
                            let
                                vDiffs = calcDiff v nv
                                res2 = uneval venv e nv vDiffs
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


logic : WS -> Expr -> Expr -> VEnv -> Value -> Bop -> List (DiffOp Value) -> UnEvalRes
logic ws e1 e2 venv newv op diffs =
    let
        en1 = eval venv e1
        v1 = getValueFromExprNode en1
        
        en2 = eval venv e2
        v2 = getValueFromExprNode en2

        en = eval venv (EBPrim ws op e1 e2)
        v = getValueFromExprNode en
        
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
            let
                v1Diffs = calcDiff v1 newv1
                v2Diffs = calcDiff v2 newv2
            in
                checkChange venv ws op e1 e2 v1 v2 newv1 newv2 v1Diffs v2Diffs


arith : WS -> Expr -> Expr -> VEnv -> Value -> Bop -> List (DiffOp Value) -> UnEvalRes
arith ws e1 e2 venv newv op diffs =
    let
        en1 = eval venv e1 
        v1 = getValueFromExprNode en1
        
        en2  = eval venv e2
        v2 = getValueFromExprNode en2
        -- _ = Debug.log "e1" <| Debug.toString e1
        -- _ = Debug.log "v1" <| Debug.toString v1
        -- _ = Debug.log "e2" <| Debug.toString e2
        -- _ = Debug.log "v2" <| Debug.toString v2

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
                    let
                        v1Diffs = calcDiff v1 newv1
                        v2Diffs = calcDiff v2 newv2
                    in
                        checkChange venv ws op e1 e2 v1 v2 newv1 newv2 v1Diffs v2Diffs

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
                    let
                        v1Diffs = calcDiff v1 newv1
                        v2Diffs = calcDiff v2 newv2
                    in                
                        checkChange venv ws op e1 e2 v1 v2 newv1 newv2 v1Diffs v2Diffs

        VCons id _ _ ->
            case op of
                Cat ->
                    let
                        (v1Diffs, v2Diffs) = splitDiffs (vConsToList v1) (vConsToList v2) diffs
                        newv1 = applyDiffs v1 v1Diffs
                        newv2 = applyDiffs v2 v2Diffs
                    in
                        checkChange venv ws op e1 e2 v1 v2 newv1 newv2 v1Diffs v2Diffs

                Add -> 
                    if id == vsId then 
                        let 
                            (v1Diffs, v2Diffs) = splitDiffs (vConsToList v1) (vConsToList v2) diffs
                            
                            -- _ = Debug.log "(v1, v2) in add" <| Debug.toString (v1, v2)
                            -- _ = Debug.log "(v1Diffs, v2Diffs) in add" <| Debug.toString (v1Diffs, v2Diffs)
                            -- _ = Debug.log "diffs in add" <| Debug.toString diffs

                            newv1 = applyDiffs v1 v1Diffs
                            newv2 = applyDiffs v2 v2Diffs
                        in 
                            checkChange venv ws op e1 e2 v1 v2 newv1 newv2 v1Diffs v2Diffs
                    else 
                        { venv = []
                        , expr = EError "Arith Expression Modified Value Type Error: 01."
                        }

                _ -> 
                    { venv = []
                    , expr = EError "Arith Expression Modified Value Type Error: 01."
                    }

        VNil _ ->
            if op == Cat || op == Add then
                let 
                    (v1Diffs, v2Diffs) = splitDiffs (vConsToList v1) (vConsToList v2) diffs
                    newv1 = applyDiffs v1 v1Diffs
                    newv2 = applyDiffs v2 v2Diffs
                in 
                checkChange venv ws op e1 e2 v1 v2 newv1 newv2 v1Diffs v2Diffs
            else
                { venv = []
                , expr = EError "Arith Expression Modified Value Type Error: 02."
                }

        _ -> 
            { venv = []
            , expr = EError "Arith Expression Modified Value Type Error: 03."
            }


comp : WS -> Expr -> Expr -> VEnv -> Value -> Bop -> List (DiffOp Value) -> UnEvalRes
comp ws e1 e2 venv newv op diffs =
    let
        en1 = eval venv e1
        v1 = getValueFromExprNode en1
        
        en2 = eval venv e2
        v2 = getValueFromExprNode en2
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
                            let
                                v1Diffs = calcDiff v1 newv1
                                v2Diffs = calcDiff v2 newv2
                            in      
                                checkChange venv ws op e1 e2 v1 v2 newv1 newv2 v1Diffs v2Diffs

                _ ->
                    let
                        resEN = eval venv (EBPrim ws op e1 e2)
                        res = getValueFromExprNode resEN
                        
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
                resEN = eval venv (EBPrim ws op e1 e2)
                res = getValueFromExprNode resEN
                
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
            Expr -> Expr -> Value -> Value -> Value -> Value -> 
            List (DiffOp Value) -> List (DiffOp Value) -> UnEvalRes
checkChange venv ws op e1 e2 v1 v2 newv1 newv2 v1Diffs v2Diffs =
    if newv1 == v1 && newv2 == v2 then
        { venv = venv
        , expr = EBPrim ws op e1 e2
        }
    else if newv1 == v1 then
        let
            res2 = uneval venv e2 newv2 v2Diffs
        in
            { venv = res2.venv
            , expr = EBPrim ws op e1 res2.expr
            }
    else if newv2 == v2 then
        let
            res1 = uneval venv e1 newv1 v1Diffs
        in
            { venv = res1.venv
            , expr = EBPrim ws op res1.expr e2
            }
    else
        let
            res1 = uneval venv e1 newv1 v1Diffs

            res2 = uneval venv e2 newv2 v2Diffs

            newvenv =
                mergeVEnv res1.venv res2.venv venv

        in
            { venv = newvenv
            , expr = EBPrim ws op res1.expr res2.expr
            }


mapUneval : VEnv -> Expr -> Expr -> Value -> List (DiffOp Value) -> (VEnv, Expr, Expr)
mapUneval venv fExpr xsExpr newv diffs =
    let        
        xsValue = getValueFromExprNode (eval venv xsExpr)
        headInputValue = 
            case xsValue of
                VCons _ v1 _ -> v1
                _ -> VError "Head Input Value doesn't exists."

        res1 = mapWalk venv fExpr xsValue newv diffs headInputValue
        newvenv1 = res1.venv
        newfExpr = res1.fExpr
        newxsValue = res1.xsValue
        xsDiffs = res1.diffs

        res2 = uneval venv xsExpr newxsValue xsDiffs
        newvenv2 = res2.venv
        newxsExpr = res2.expr

        newvenv = mergeVEnv newvenv1 newvenv2 venv

        _ = Debug.log "mapUneval-(xsExpr, newv)" <| Debug.toString (xsExpr, newv)
        _ = Debug.log "mapUneval-newxsValue" <| Debug.toString newxsValue
        _ = Debug.log "mapUneval-xsDiffs" <| Debug.toString xsDiffs

    in
        (newvenv, newfExpr, newxsExpr)
    

mapWalk : VEnv -> Expr -> Value -> Value -> List (DiffOp Value) -> Value -> MapWalkRes
mapWalk venv fExpr xsValue newv diffs headOrPreviousInputValue =
    let
        errMsg = "Map Uneval Error." ++ Debug.toString (xsValue, newv, diffs)
        errRes =
            { venv = []
            , fExpr = EError errMsg
            , xsValue = VError errMsg
            , diffs = []
            } 
        
    in
        case (diffs, xsValue, newv) of
            ([], VNil _, VNil _) -> 
                { venv = venv
                , fExpr = fExpr
                , xsValue = xsValue
                , diffs = []
                } 

            ((DiffDelete _) :: restDiffs, VCons _ v1 v2, _) ->
                let
                    res = mapWalk venv fExpr v2 newv restDiffs v1
                    newvenv = res.venv
                    newfExpr = res.fExpr
                    newxsValue = res.xsValue
                    newDiffs = res.diffs
                in
                    { venv = newvenv
                    , fExpr = newfExpr
                    , xsValue = newxsValue
                    , diffs = (DiffDelete v1) :: newDiffs
                    }
            
            ((DiffInsert newv1) :: restDiffs, _, VCons _ _ newv2) ->
                let 
                    -- convert value to expr
                    olde = valueToExpr headOrPreviousInputValue
                    applyF = EApp defaultWS fExpr olde

                    oldv1 = getValueFromExprNode (eval venv applyF)
                    v1Diffs = calcDiff oldv1 newv1
                    -- update: v |> f headOrPreviousInputValue 
                    res1 = uneval venv applyF newv1 v1Diffs
                    
                    newvenv1 = res1.venv
                    (newfExpr1, newxExpr) = 
                        case res1.expr of
                            EApp _ f1 x1 -> (f1, x1)
                            _ -> (fExpr, EError "Function f update error in map.")
                    newxValue = getValueFromExprNode (eval venv newxExpr)

                    res2 = mapWalk venv fExpr xsValue newv2 restDiffs headOrPreviousInputValue 
                    newvenv2 = res2.venv
                    newfExpr2 = res2.fExpr
                    newxsValue = res2.xsValue
                    newDiffs = res2.diffs

                    -- merge result
                    newvenv = mergeVEnv newvenv1 newvenv2 venv
                    newfExpr = mergeFunc newfExpr1 newfExpr2 fExpr

                    newvid = getVConsId xsValue

                    _ = Debug.log "mapWalk-olde" <| Debug.toString olde
                    _ = Debug.log "mapWalk-newx" <| Debug.toString newxValue
                    
                in
                    { venv = newvenv
                    , fExpr = newfExpr
                    , xsValue = VCons newvid newxValue newxsValue
                    , diffs = (DiffInsert newxValue) :: newDiffs
                    }

            ((DiffKeep _) :: restDiffs, VCons vid v1 v2, VCons _ _ newv2) ->
                let 
                    res = mapWalk venv fExpr v2 newv2 restDiffs v1
                    newvenv = res.venv
                    newfExpr = res.fExpr
                    newxsValue = res.xsValue
                    newDiffs = res.diffs
                in
                    { venv = newvenv
                    , fExpr = newfExpr
                    , xsValue = VCons vid v1 newxsValue
                    , diffs = (DiffKeep v1) :: newDiffs
                    }
            
            ((DiffUpdate newv1) :: restDiffs, VCons vid v1 v2, VCons _ _ newv2) ->
                let
                    olde = valueToExpr v1
                    applyF = EApp defaultWS fExpr olde

                    oldv1 = getValueFromExprNode (eval venv applyF)
                    v1Diffs = calcDiff oldv1 newv1
                    res1 = uneval venv applyF newv1 v1Diffs

                    newvenv1 = res1.venv
                    (newfExpr1, newxExpr) = 
                        case res1.expr of
                            (EApp _ f1 x1) -> (f1, x1)
                            _ -> (fExpr, EError "Function f update error in map.")
                    newxValue = getValueFromExprNode (eval venv newxExpr)

                    res2 = mapWalk venv fExpr v2 newv2 restDiffs v1
                    newvenv2 = res2.venv
                    newfExpr2 = res2.fExpr
                    newxsValue = res2.xsValue
                    newDiffs = res2.diffs

                    newvenv = mergeVEnv newvenv1 newvenv2 venv
                    newfExpr = mergeFunc newfExpr1 newfExpr2 fExpr
                in
                    { venv = newvenv
                    , fExpr = newfExpr
                    , xsValue = VCons vid newxValue newxsValue
                    , diffs = (DiffUpdate newxValue) :: newDiffs
                    }

            _ -> errRes

flattenUneval : VEnv -> Expr -> Value -> List (DiffOp Value) -> (VEnv, Expr)
flattenUneval venv xss newv diffs = 
    ([], EError "")