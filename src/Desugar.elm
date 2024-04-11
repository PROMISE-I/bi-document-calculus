module Desugar exposing (..)

import Syntax exposing (..)
import LangUtils exposing (..)

desugarWithPreclude : Expr -> Expr
desugarWithPreclude expr = 
    -- let
        -- _ = Debug.log "desugar" <| Debug.toString (desugar expr)
    -- in
    
    withPreclude (desugar expr)

desugar : Expr -> Expr
desugar expr = 
    case expr of 
        EVar ws s -> EVar ws s

        ELam ws p e -> 
            let
                de = desugar e
            in
                ELam ws p de            

        ELet ws p e1 e2 -> 
            let
                de1 = desugar e1
                de2 = desugar e2
            in
                ELet ws p de1 de2

        ELetrec ws p e1 e2 -> 
            let
                de1 = desugar e1
                de2 = desugar e2
            in
                ELetrec ws p de1 de2
            
        EApp ws e1 e2 -> 
            let
                de1 = desugar e1
                de2 = desugar e2
            in
                EApp ws de1 de2

        EInt ws n -> EInt ws n

        EFloat ws n -> EFloat ws n

        ETrue ws -> ETrue ws
        
        EFalse ws -> EFalse ws
        
        EChar ws c -> EChar ws c
        
        ECons ws e1 e2 -> 
            let
                de1 = desugar e1
                de2 = desugar e2
            in
                ECons ws de1 de2
        
        EBTuple ws e1 e2 -> 
            let
                de1 = desugar e1
                de2 = desugar e2
            in
                EBTuple ws de1 de2

        ETTuple ws e1 e2 e3 -> 
            let
                de1 = desugar e1
                de2 = desugar e2
                de3 = desugar e3
            in
                ETTuple ws de1 de2 de3

        ENil ws -> ENil ws

        EUPrim ws op e -> 
            let
                de = desugar e
            in
                EUPrim ws op de

        EBPrim ws op e1 e2 -> 
            let
                de1 = desugar e1
                de2 = desugar e2
            in
                EBPrim ws op de1 de2

        ECase ws e branch -> 
            let
                de = desugar e
                dbranch = desugarBranch branch
            in
                ECase ws de dbranch
            
        EFix ws e -> 
            let
                de = desugar e
            in
                EFix ws de
        
        EParens ws e -> 
            let
                de = desugar e
            in
                EParens ws de            

        StrTpl t -> 
            (
                EApp
                    defaultWS
                    (EApp defaultWS eVarJoin empStrExpr)
                    (desugarTemplate headWS t)
            )
        
        TreeTpl ws t ->
            (
                EApp
                    defaultWS
                    lamTreeTpl
                    (
                        ENode 
                            ws
                            "div"
                            (ENil ([], eoElm))
                            (desugarTemplate headWS t)
                    )
            )

        ENode ws s e1 e2 -> 
            let
                de1 = desugar e1
                de2 = desugar e2
            in
                ENode ws s de1 de2

        EToStr ws e -> 
            let
                de = desugar e
            in
                EToStr ws de

        EError info -> EError info


desugarBranch : Branch -> Branch
desugarBranch branch =
    case branch of
        BSin ws p e -> 
            let
                de = desugar e
            in
                BSin ws p de

        BNSin ws n p e ->
            let
                de = desugar e
            in
                BNSin ws n p de
        
        BCom ws b1 b2 -> 
            let 
                db1 = desugarBranch b1
                db2 = desugarBranch b2
            in 
                BCom ws db1 db2
                        

desugarTemplate : WS -> Template -> Expr
desugarTemplate ws t = 
    case t of
        TCons ctx tPart restTemplate -> 
            let
                desugaredRestTemplate = desugarTemplate tailWS restTemplate
            in
            
            case tPart of
                TplStr e -> 
                    -- make unique identification for resugar
                    EApp 
                        defaultWS
                        lamTplStr
                        (ECons ws e desugaredRestTemplate)
                                
                TplNode tpWS n e t1 -> 
                    let
                        desugaredAttr = desugar e
                        desugaredT1 = desugarTemplate headWS t1
                        desugaredTplNode = ENode defaultWS n desugaredAttr desugaredT1
                    in
                    -- make unique identification for resugar
                        EApp
                            tpWS
                            lamTplNode
                            (ECons ws desugaredTplNode desugaredRestTemplate)

                TplExpr tpWS e -> 
                    -- make unique identification for resugar
                    EApp 
                        tpWS
                        lamTplExpr       
                        (ECons ws e desugaredRestTemplate) -- TODO: 忘记 deusgar e 了

                TplSet tpWS p e -> 
                    -- make unique identification for resugar
                    EApp
                        tpWS
                        lamTplSet
                        (ELet defaultWS p e desugaredRestTemplate)

                TplIf tpWS e t1 t2 -> 
                    let
                        desugaredT1 = desugarTemplate headWS t1
                        desugaredT2 = desugarTemplate headWS t2
                        ifSpliceT = 
                            TCons 
                                ctx
                                (
                                    TplSplice
                                        (
                                            EApp
                                                defaultWS
                                                (
                                                    ELam 
                                                        defaultWS
                                                        (PVar defaultWS caseN)
                                                        (
                                                            ECase 
                                                                defaultWS
                                                                (EVar defaultWS caseN)
                                                                (
                                                                    BCom 
                                                                        defaultWS 
                                                                        (BSin defaultWS (PTrue defaultWS) desugaredT1)
                                                                        (BSin defaultWS (PFalse defaultWS) desugaredT2)
                                                                )
                                                        )
                                                )
                                                e
                                        )
                                )
                                restTemplate
                    in         
                    -- make unique identification for resugar
                        EApp
                            tpWS
                            lamTplIf        
                            (desugarTemplate ws ifSpliceT)
                    
                TplForeach tpWS p e t1 -> 
                    let
                        desugaredT1 = desugarTemplate headWS t1
                        foreachSpliceT =                         
                            TCons 
                                ctx
                                (
                                    TplSplice
                                        (
                                            EApp
                                                defaultWS
                                                eVarFlatten
                                                (
                                                    EApp
                                                        defaultWS
                                                        (
                                                            EApp 
                                                                defaultWS 
                                                                eVarMap
                                                                (ELam defaultWS p desugaredT1)
                                                        )
                                                        e
                                                )
                                        )
                                )
                                restTemplate
                    in
                    -- make unique identification for resugar
                        EApp 
                            tpWS
                            lamTplForeach
                            (desugarTemplate ws foreachSpliceT)
                    
                TplSplice e -> 
                    EApp
                        defaultWS
                        (EApp defaultWS eVarAppend e)
                        desugaredRestTemplate


        TNil _ -> ENil ws