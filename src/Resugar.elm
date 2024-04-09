module Resugar exposing (..)
import Syntax exposing (..)
import LangUtils exposing (..)
import Parser_ exposing (..)

resugarWithoutPreclude : Expr -> Expr
resugarWithoutPreclude e =
    resugar (removePreclude e)


removePreclude : Expr -> Expr
removePreclude e =
    case e of
        ELetrec 
            _ 
            (PVar _ apd) 
            lamApd 
            (
                ELetrec
                    _
                    (PVar _ flat)
                    lamFlat
                    (
                        ELetrec 
                            _
                            (PVar _ mp)
                            lamMp
                            (
                                ELetrec 
                                    _
                                    (PVar _ joi)
                                    lamJoi
                                    userExpr
                            )
                    )
            ) ->
            if 
                apd == appendName && lamApd == appendBody &&
                flat == flattenName && lamFlat == flattenBody &&
                mp == mapName && lamMp == mapBody &&
                joi == joinName && lamJoi == joinBody    
            then
                userExpr
            else 
                EError ("Not Allow Modification on Output, which cause updates on pre-define functions:\n" ++ (Debug.toString e))
        _ -> EError ("Not Allow Modification on Output, which cause updates on pre-define functions:\n" ++ (Debug.toString e))


resugar : Expr -> Expr
resugar expr =
    case expr of 
        EVar ws s -> EVar ws s

        ELam ws p e -> ELam ws p (resugar e)

        ELet ws p e1 e2 -> ELet ws p (resugar e1) (resugar e2)

        ELetrec ws p e1 e2 -> ELetrec ws p (resugar e1) (resugar e2)

        EApp ws e1 e2 -> 
            if e1 == (EApp defaultWS eVarJoin empStrExpr) then
                let
                    tRes = resugarTemplatePart stctx e2
                in
                    case tRes of
                        Result.Ok t -> StrTpl t
                        Result.Err info -> EError info
            else 
                EApp ws (resugar e1) (resugar e2)

        EInt ws n -> EInt ws n

        EFloat ws n -> EFloat ws n

        ETrue ws -> ETrue ws
        
        EFalse ws -> EFalse ws
        
        EChar ws c -> EChar ws c
        
        ECons ws e1 e2 -> ECons ws (resugar e1) (resugar e2)
        
        EBTuple ws e1 e2 -> EBTuple ws (resugar e1) (resugar e2)

        ETTuple ws e1 e2 e3 -> ETTuple ws (resugar e1) (resugar e2) (resugar e3)

        ENil ws -> ENil ws

        EUPrim ws op e -> EUPrim ws op (resugar e)

        EBPrim ws op e1 e2 -> EBPrim ws op (resugar e1) (resugar e2)

        ECase ws e branch -> ECase ws (resugar e) (resugarBranch branch)

        EFix ws e -> EFix ws (resugar e)
        
        EParens ws e -> EParens ws (resugar e)

        ENode ws s e1 e2 -> ENode ws s (resugar e1) (resugar e2)

        EToStr ws e -> EToStr ws (resugar e)

        _ -> EError ("Resugar Template Error: 01\n" ++ (Debug.toString expr))
        
resugarBranch : Branch -> Branch
resugarBranch branch = 
    case branch of 
        BSin ws p e -> BSin ws p (resugar e)

        BNSin ws n p e -> BNSin ws n p (resugar e)
        
        BCom ws b1 b2 -> BCom ws (resugarBranch b1) (resugarBranch b2)

resugarTemplatePart : TplCtx -> Expr -> Result String Template
resugarTemplatePart ctx expr =
    case expr of
        -- All template parts desugar to EApp of e1 and e2, where e1 is identity and e2 is the real expr
        EApp ws e1 e2 -> 
            if e1 == lamTplStr then
                case e2 of 
                    ECons _ userStr restExpr -> 
                        let
                            restTPartsRes = resugarTemplatePart ctx restExpr
                        in
                            case restTPartsRes of
                                Result.Ok restTemplate -> 
                                    Result.Ok (TCons ctx (TplStr userStr) restTemplate)

                                Result.Err info -> Result.Err info

                    _ -> Result.Err "Resugar Template Part Error: 03"


            else if e1 == lamTplExpr then
                case e2 of
                    ECons _ userExpr restExpr ->
                        let
                            restTPartsRes = resugarTemplatePart ctx restExpr
                        in
                            case restTPartsRes of
                                Result.Ok restTemplate ->
                                    Result.Ok (TCons ctx (TplExpr ws userExpr) restTemplate)

                                Result.Err info -> Result.Err info

                    _ -> Result.Err "Resugar Template Part Error: 04"


            else if e1 == lamTplSet then
                case e2 of
                    ELet _ p e restExpr ->
                        let
                            restTPartsRes = resugarTemplatePart ctx restExpr
                        in
                            case restTPartsRes of
                                Result.Ok restTemplate ->
                                    Result.Ok (TCons ctx (TplSet ws p e) restTemplate)

                                Result.Err info -> Result.Err info
                    
                    _ -> Result.Err "Resugar Template Part Error: 04"
                        

            else if e1 == lamTplIf then
                case e2 of
                    -- If template part is warpped by splice,
                    -- which is the form of `EApp _ (EApp _ (EVar _ "$append$")) eIf`,
                    -- and eIf is the real part of if template part.
                    EApp _ (EApp _ (EVar _ apd) eIf) restExpr ->
                        if apd == appendName then 
                            let
                                restTPartsRes = resugarTemplatePart ctx restExpr
                            in
                                case restTPartsRes of
                                    Result.Ok restTParts ->
                                        case eIf of
                                            EApp 
                                                _ 
                                                (
                                                    ELam 
                                                        _ 
                                                        (PVar _ cName1) 
                                                        (
                                                            ECase 
                                                                _ 
                                                                (EVar _ cName2)
                                                                (
                                                                    BCom
                                                                        _
                                                                        (BSin _ (PTrue _) thenExpr)
                                                                        (BSin _ (PFalse _) elseExpr) 
                                                                )
                                                        )
                                                ) 
                                                condExpr ->
                                                    if cName1 == caseN && cName2 == caseN then  
                                                        let
                                                            thenTplRes = resugarTemplatePart ctx thenExpr
                                                            elseTplRes = resugarTemplatePart ctx elseExpr
                                                        in
                                                            case thenTplRes of
                                                                Result.Ok thenTpl ->
                                                                    case elseTplRes of
                                                                        Result.Ok elseTpl ->
                                                                            let 
                                                                                ifTplPart = TplIf ws condExpr thenTpl elseTpl
                                                                            in
                                                                                Result.Ok (TCons ctx ifTplPart restTParts)
                                                                    
                                                                        Result.Err info -> Result.Err info
                                                                
                                                                Result.Err info -> Result.Err info

                                                    else 
                                                        Result.Err "Resugar Template Part Error: 06"

                                            _ -> Result.Err "Resugar Template Part Error: 04"

                                    Result.Err info -> Result.Err info
                                   
                        else 
                            Result.Err "Resugar Template Part Error: 05"

                    _ -> Result.Err "Resugar Template Part Error: 04"


            else if e1 == lamTplForeach then
                case e2 of
                    -- Foreach template part is warpped by splice,
                    -- which is the form of `EApp _ (EApp _ (EVar _ "$append$")) eForeach`,
                    -- and eForeach is the real part of foreach template part.
                    EApp _ (EApp _ (EVar _ apd) eForeach) restExpr ->
                        if apd == appendName then 
                            let
                                restTPartsRes = resugarTemplatePart ctx restExpr
                            in
                                case restTPartsRes of
                                    Result.Ok restTParts ->
                                        case eForeach of
                                            EApp 
                                                _
                                                (EVar _ flat)
                                                (
                                                    EApp
                                                        _
                                                        (
                                                            EApp
                                                                _
                                                                (EVar _ mp)
                                                                (ELam _ p foreachBodyExpr)
                                                        )
                                                        e
                                                ) ->
                                                    if flat == flattenName && mp == mapName then
                                                        let
                                                            foreachBodyTplRes = resugarTemplatePart ctx foreachBodyExpr
                                                        in
                                                            case foreachBodyTplRes of
                                                                Result.Ok foreachBodyTpl ->
                                                                    let
                                                                        foreachTplPart = TplForeach ws p e foreachBodyTpl
                                                                    in
                                                                    
                                                                        Result.Ok (TCons ctx foreachTplPart restTParts)

                                                                Result.Err info -> Result.Err info

                                                    else 
                                                        Result.Err "Resugar Template Part Error: 06"

                                            _ -> Result.Err "Resugar Template Part Error: 04"
                                    
                                    Result.Err info -> Result.Err info
                        
                        else
                            Result.Err "Resugar Template Part Error: 05"
                    
                    _ -> Result.Err "Resugar Template Part Error: 04"


            else 
                Result.Err "Resugar Template Part Error: 02" 
                
        ENil _ -> Result.Ok (TNil ctx)

        _ -> Result.Err "Resugar Template Part Error: 01"
