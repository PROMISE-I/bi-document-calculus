module BDDesugar exposing (..)

import BDSyntax exposing (..)
import BDLangUtils exposing (..)

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
                    (desugarTemplate t)
            )

        EHtml ws s e1 e2 e3 -> 
            let
                de1 = desugar e1
                de2 = desugar e2
                de3 = desugar e3
            in
                EHtml ws s de1 de2 de3

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
                        

desugarTemplate : Template -> Expr
desugarTemplate t = 
    case t of
        TCons ctx _ _ -> 
            if ctx == stctx then
                desugarStringTemplate t
            else 
                deusgarArticleTemplate t

        TNil _ -> ENil (["", " "], eoAddFromEmp)


desugarStringTemplate : Template -> Expr
desugarStringTemplate t = 
    case t of 
        TCons ctx tPart restTemplate ->
            let
                desugaredRestTemplate = desugarStringTemplate restTemplate
            in
            
            case tPart of
                TplStr e -> ECons ([" "], eoElm) e desugaredRestTemplate

                TplExpr e -> ECons ([" "], eoElm) e desugaredRestTemplate

                TplSet ws p e -> ELet ws p e desugaredRestTemplate

                TplIf ws e t1 t2 -> 
                    let
                        desugaredT1 = desugarStringTemplate t1
                        desugaredT2 = desugarStringTemplate t2
                        ifSpliceT = 
                            TCons 
                                ctx
                                (
                                    TplSplice
                                        (
                                            EApp
                                                ws
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
                        desugarStringTemplate ifSpliceT
                    
                TplForeach ws p e t1 -> 
                    let
                        desugaredT1 = desugarStringTemplate t1
                        foreachSpliceT = 
                            TCons 
                                ctx
                                (
                                    TplSplice
                                        (
                                            EApp
                                                ws
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
                        desugarStringTemplate foreachSpliceT
                    
                TplSplice e -> 
                    EApp
                        defaultWS
                        (EApp defaultWS eVarAppend e)
                        desugaredRestTemplate
                    

        TNil _ -> ENil ([], eoElm)


deusgarArticleTemplate : Template -> Expr
deusgarArticleTemplate t =
    EError "Error: Desugar article template has not been implemented."