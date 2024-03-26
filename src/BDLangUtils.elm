module BDLangUtils exposing (..)


import BDUtils exposing (..)
import BDSyntax exposing (..)
import Debug exposing (toString)

printAST : Expr -> String
printAST expr =
    case expr of
        EVar ([ws], _) s ->
            s ++ ws

        ELam ([ws1, ws2], _) p e ->
            "\\" ++ ws1 ++ (printPattern p) ++ "=>" ++ ws2 ++ (printAST e)

        ELam _ (PVar _ "$CASE$") e ->
            printAST e
        
        ELet ([ws1, ws2, ws3], _) p e1 e2 ->
            "let" ++ ws1 ++ (printPattern p) ++ "=" ++ ws2 ++
            (printAST e1) ++ "in" ++ ws3 ++ (printAST e2)
        
        ELetrec ([ws1, ws2, ws3], _) p e1 e2 ->
            "letrec" ++ ws1 ++ (printPattern p) ++ "=" ++ ws2 ++
            (printAST e1) ++ "in" ++ ws3 ++ (printAST e2)

        EApp ([ws1, ws2], 1) e1 e2 ->
            "case" ++ ws1 ++ (printAST e2) ++ "of" ++ ws2 ++ (printAST e1)

        EApp ([ws], 2) e1 e2 ->
            "if" ++ ws ++ (printAST e2) ++ (printAST e1)

        EApp _ e1 e2 ->
            (printAST e1) ++ (printAST e2)
        
        EInt ([ws], _) n ->
            (toString n) ++ ws
        
        EInt _ n ->
            (toString n) ++ " "
        
        EFloat ([ws], _) n ->
            (toString n) ++ ws

        EFloat _ n ->
            (toString n) ++ " "
        
        ETrue ([ws], _) ->
            "true" ++ ws
        
        ETrue _ ->
            "true "

        EFalse ([ws], _) ->
            "false" ++ ws

        EFalse _ ->
            "false "

        EChar ([ws], _) c ->
            "\'" ++ (String.fromChar c) ++ "\'" ++ ws

        EChar _ c ->
            "\'" ++ (String.fromChar c) ++ "\' "
        
        ECons (ls, kind) e1 e2 ->
            case (ls, kind) of
                ([ws1, ws2], 0) ->
                    "[" ++ ws1 ++ (printAST e1) ++
                    (printAST e2) ++ "]" ++ ws2
                
                ([ws], 1) ->
                    "," ++ ws ++ (printAST e1) ++ (printAST e2)
                
                (_, 1) ->
                    ", " ++ (printAST e1) ++ (printAST e2)
                
                ([ws], 2) ->
                    (printAST e1) ++ "::" ++ ws ++ (printAST e2)

                ([ws], 3) ->
                    case e1 of
                        EChar _ c ->
                            "\""++ (String.fromChar c) ++ (printAST e2) ++ "\"" ++ ws

                        _ -> "Print Error: 05."
                
                ([], 4) ->
                    case e1 of
                        EChar _ c ->
                            (String.fromChar c) ++ (printAST e2)

                        _ -> "Print Error: 06."

                _ ->
                    "Print Error: 04."

        ENil ([ws], 3) ->
            "\"\"" ++ ws
        
        ENil (_, 3) ->
            "\"\" "


        ENil ([ws1, ws2], _) ->
            "[" ++ ws1 ++ "]" ++ ws2

        ENil _ ->
            ""

        EBPrim ([ws], _) op e1 e2 ->
            let
                s1 =
                    printAST e1

                s2 =
                    printAST e2
                
                sop =
                    case op of
                        Add -> "+"
                        Sub -> "-"
                        Mul -> "*" 
                        Div -> "//"
                        DDiv -> "/"
                        Eq  -> "=="
                        Lt  -> "<"
                        Gt  -> ">"
                        Le  -> "<="
                        Ge  -> ">="
                        And -> "&&"
                        Or  -> "||"
                        Cat -> "++"
            in
                s1 ++ sop ++ ws ++ s2
            
        EUPrim ([ws], 0) op e ->
            let
                s =
                    printAST e
                
                sop =
                    case op of
                        Neg -> "-"
                        Not -> "!"
            in
                sop ++ ws ++ s

        ECase ([ws1, ws2], 1) _ (BCom _ (BSin _ _ e1) (BSin _ _ e2)) ->
            "then" ++ ws1 ++ (printAST e1) ++ "else" ++ ws2 ++ (printAST e2)

        ECase _ _ branch ->
            printBranch branch

        EFix _ e ->
            printAST e

        EParens ([ws1, ws2], _) e ->
            "(" ++ ws1 ++ (printAST e) ++ ")" ++ ws2

        EBTuple ([ws1, ws2, ws3], _) e1 e2 ->
            "(" ++ ws1 ++ (printAST e1) ++ "," ++ ws2 ++ (printAST e2) ++ ")" ++ ws3 

        ETTuple ([ws1, ws2, ws3, ws4], _) e1 e2 e3 ->
            "(" ++ ws1 ++ (printAST e1) ++ "," ++ ws2 ++ (printAST e2) ++
            "," ++ ws3 ++ (printAST e3) ++ ")" ++ ws4
        
        StrTpl t ->
            "{#" ++ (printTpl t) ++ "#}"

        EHtml ([ws1, ws2, ws3], 0) s e1 e2 e3 ->
            "Html." ++ s ++ ws1 ++ (printAST e1) ++ (ws2) ++
            (printAST e2) ++ ws3 ++ (printAST e3)

        EToStr ([ws], 0) e ->
            "toString" ++ ws ++ (printAST e)

        EError info ->
            info
        
        _ -> "Print Error: 01."


printPattern : Pattern -> String
printPattern p =
    case p of
        PVar ([ws], _) s ->
            s ++ ws

        PCons (ls, kind) p1 p2 ->
            case (ls, kind) of
                ([ws1, ws2], 0) ->
                    "[" ++ ws1 ++ (printPattern p1) ++
                    (printPattern p2) ++ "]" ++ ws2
                
                ([ws], 1) ->
                    "," ++ ws ++ (printPattern p1) ++ (printPattern p2)
                
                ([ws], 2) ->
                    (printPattern p1) ++ "::" ++ ws ++ (printPattern p2)

                ([ws], 3) ->
                    case p1 of
                        PChar _ c ->
                            "\""++ (String.fromChar c) ++ (printPattern p2) ++ "\"" ++ ws

                        _ -> "Print Error: 08."
                
                ([], 4) ->
                    case p1 of
                        PChar _ c ->
                            (String.fromChar c) ++ (printPattern p2)

                        _ -> "Print Error: 09."

                _ ->
                    "Print Error: 02."
    
        PNil ([ws1, ws2], _) ->
            "[" ++ ws1 ++ "]" ++ ws2

        PNil ([ws], 3) ->
            "\"\"" ++ ws

        PNil (_, 3) ->
            "\"\" "

        PNil _ ->
            ""

        PInt ([ws], _) n ->
            (toString n) ++ ws
        
        PFloat ([ws], _) n ->
            (toString n) ++ ws
        
        PTrue ([ws], _) ->
            "true" ++ ws
        
        PFalse ([ws], _) ->
            "false" ++ ws

        PChar ([ws], _) c ->
            (String.fromChar c) ++ ws

        PBTuple ([ws1, ws2, ws3], _) p1 p2 ->
            "(" ++ ws1 ++ (printPattern p1) ++ "," ++ ws2 ++ (printPattern p2) ++ ")" ++ ws3 

        PTTuple ([ws1, ws2, ws3, ws4], _) p1 p2 p3 ->
            "(" ++ ws1 ++ (printPattern p1) ++ "," ++ ws2 ++ (printPattern p2) ++
            "," ++ ws3 ++ (printPattern p3) ++ ")" ++ ws4

        _ ->
            "Print Error: 03."


printBranch : Branch -> String
printBranch b =
    case b of
        BSin ([ws], _) p e ->
            (printPattern p) ++ "=>" ++ ws ++ (printAST e)

        BCom ([ws], _) b1 b2 ->
            (printBranch b1) ++ "|" ++ ws ++ (printBranch b2)

        _ ->
            "Print Error: 05."


printTpl : Template -> String
printTpl t =
    case t of
        TCons _ tplPart restTpl ->
            (printTplPart tplPart) ++ (printTpl restTpl)

        TNil _ -> ""

printTplPart : TplPart -> String
printTplPart tplPart = 
    case tplPart of
        TplStr s -> printAST s

        TplExpr ([ws1, ws2], _) e -> 
            "{{" ++ ws1 ++ (printAST e) ++ ws2 ++"}}"

        TplSet ([ws1, ws2, ws3, ws4, ws5], _) p e ->
            "{%" ++ ws1 ++ "set" ++ ws2 ++ (printPattern p) ++ ws3 ++ "=" ++ ws4 ++ (printAST e) ++ ws5 ++ "%}"

        TplIf ([ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8], _) e t1 t2 ->
            "{%" ++ ws1 ++ "if" ++ ws2 ++ (printAST e) ++ ws3 ++ "then" ++ ws4 ++ "%}" ++
            (printTpl t1) ++ 
            "{%" ++ ws5 ++ "else" ++ ws6 ++ "%}" ++
            (printTpl t2) ++ 
            "{%" ++ ws7 ++ "endif" ++ ws8 ++ "%}"
        
        TplForeach ([ws1, ws2, ws3, ws4, ws5, ws6, ws7], _) p e t -> 
            "{%" ++ ws1 ++ "for" ++ ws2 ++ (printPattern p) ++ ws3 ++ "in" ++ ws4 ++ (printAST e) ++ ws5 ++ "%}" ++ 
            (printTpl t) ++ 
            "{%" ++ ws6 ++ "endfor" ++ ws7 ++ "%}"

        _ -> "Print Error: 02"

-- processBeforePrint: 给 case 的 branch 去编号，BNSin -> BSin

processBeforePrint : Expr -> List String -> Expr
processBeforePrint expr env =
    case expr of
        EVar ws s ->   
            EVar ws s

        ELam ws pat body ->
            let 
                body_ = processBeforePrint body env
            in
                ELam ws pat body_

        ELet ws pat e1 e2 ->
            let
                e1_ = processBeforePrint e1 env
                
                e2_ = processBeforePrint e2 env 
            in
                ELet ws pat e1_ e2_

        ELetrec ws pat e1 e2 ->
            let

                e1_ = processBeforePrint e1 env
                
                e2_ = processBeforePrint e2 env 
            in
                ELetrec ws pat e1_ e2_

        EApp ws e1 e2 ->
            let 
                e1_ = processBeforePrint e1 env
            
                e2_ = processBeforePrint e2 env 
            in
                EApp ws e1_ e2_

        ECons ws e1 e2 ->
            let 
                e1_ = processBeforePrint e1 env
            
                e2_ = processBeforePrint e2 env 
            in
                ECons ws e1_ e2_

        EBPrim ws op e1 e2 ->
            let 
                e1_ = processBeforePrint e1 env
            
                e2_ = processBeforePrint e2 env 
            in
                EBPrim ws op e1_ e2_

        EUPrim ws op e ->
            let 
                e_ = processBeforePrint e env
            in
                EUPrim ws op e_

        ECase ws e branch ->
            let 
                e_ = processBeforePrint e env
                
                branch_ = processBranchesBeforePrint branch env 
            in
                ECase ws e_ branch_
        
        EFix ws e ->
            let 
                e_ = processBeforePrint e env
            in
                EFix ws e_

        EParens ws e ->
            let 
                e_ = processBeforePrint e env
            in
                EParens ws e_

        EBTuple ws e1 e2 ->
            let 
                e1_ = processBeforePrint e1 env
            
                e2_ = processBeforePrint e2 env 
            in
                EBTuple ws e1_ e2_

        ETTuple ws e1 e2 e3 ->
            let 
                e1_ = processBeforePrint e1 env
            
                e2_ = processBeforePrint e2 env

                e3_ = processBeforePrint e3 env
            in
                ETTuple ws e1_ e2_ e3_

        EHtml ws s e1 e2 e3 ->
            let 
                e1_ = processBeforePrint e1 env
            
                e2_ = processBeforePrint e2 env

                e3_ = processBeforePrint e3 env
            in
                EHtml ws s e1_ e2_ e3_

        EToStr ws e ->
            let
                e_ = processBeforePrint e env
            in
                EToStr ws e_

        _ ->
            expr


processBranchesBeforePrint : Branch -> List String -> Branch
processBranchesBeforePrint b env =
    case b of
        BNSin ws _ pat e ->
            let 
                e_ = processBeforePrint e env
            in
                BSin ws pat e_

        BCom ws b1 b2 ->
            let 
                b1_ = processBranchesBeforePrint b1 env

                b2_ = processBranchesBeforePrint b2 env 
            in
                BCom ws b1_ b2_

        BSin _ _ _ -> b


-- processAfterParse: 给 case 的 branch 编号，BSin -> BNSin

processAfterParse : Expr -> List String -> Expr
processAfterParse expr env =
    case expr of

        EVar ws s ->
            EVar ws s

        ELam ws pat body ->
            let 
                body_ = processAfterParse body env
            in
                ELam ws pat body_

        ELet ws pat e1 e2 ->
            let
                e1_ = processAfterParse e1 env
                
                e2_ = processAfterParse e2 env
            in
                ELet ws pat e1_ e2_

        ELetrec ws pat e1 e2 ->
            let
                e1_ = processAfterParse e1 env
                
                e2_ = processAfterParse e2 env
            in
                ELetrec ws pat e1_ e2_

        EApp ws e1 e2 ->
            let 
                e1_ = processAfterParse e1 env 

                e2_ = processAfterParse e2 env
            in
                EApp ws e1_ e2_

        ECons ws e1 e2 ->
            let 
                e1_ = processAfterParse e1 env 

                e2_ = processAfterParse e2 env
            in
                ECons ws e1_ e2_

        EBPrim ws op e1 e2 ->
            let 
                e1_ = processAfterParse e1 env 

                e2_ = processAfterParse e2 env
            in
                EBPrim ws op e1_ e2_

        EUPrim ws op e ->
            let 
                e_ = processAfterParse e env 
            in
                EUPrim ws op e_

        ECase ws e branch ->
            let 
                e_ = processAfterParse e env 

                (branch_, _) = 
                    numberBranches branch env 0 
            in
                ECase ws e_ branch_
        
        EFix ws e ->
            let 
                e_ = processAfterParse e env
            in
                EFix ws e_

        EParens ws e ->
            let 
                e_ = processAfterParse e env 
            in
                EParens ws e_

        EBTuple ws e1 e2 ->
            let 
                e1_ = processAfterParse e1 env 
                
                e2_ = processAfterParse e2 env
            in
                EBTuple ws e1_ e2_

        ETTuple ws e1 e2 e3 ->
            let 
                e1_ = processAfterParse e1 env 

                e2_ = processAfterParse e2 env

                e3_ = processAfterParse e3 env
            in
                ETTuple ws e1_ e2_ e3_

        EHtml ws s e1 e2 e3 ->
            let 
                e1_ = processAfterParse e1 env 

                e2_ = processAfterParse e2 env

                e3_ = processAfterParse e3 env
            in
                EHtml ws s e1_ e2_ e3_

        EToStr ws e ->
            let
                e_ = processAfterParse e env
            in
                EToStr ws e_

        _ -> expr


numberBranches : Branch -> List String -> Int -> (Branch, Int)
numberBranches b env n =
    case b of
        BSin ws pat e ->           
            let 
                e_ = processAfterParse e env 
            in
                (BNSin ws n pat e_, n+1)

        BCom ws b1 b2 ->
            let 
                (b1_, n1) = numberBranches b1 env n 

                (b2_, n2) = numberBranches b2 env n1
            in
                (BCom ws b1_ b2_, n2)

        BNSin _ _ _ _ -> (b, n)

match : Pattern -> Value -> VEnv
match pat val =
    case (pat, val) of
        ((PCons _ p1 p2), (VCons _ v vs)) ->
            let
                res1 = match p1 v

                res2 = match p2 vs
            in
            if res1 == [("ERROR", VError "Match Failed.")] ||
                res2 == [("ERROR", VError "Match Failed.")]
            then 
                [("ERROR", VError "Match Failed.")]
            else
                res1++res2

        ((PBTuple _ p1 p2), (VBTuple v1 v2)) ->
            let
                res1 = match p1 v1

                res2 = match p2 v2
            in
            if res1 == [("ERROR", VError "Match Failed.")] ||
                res2 == [("ERROR", VError "Match Failed.")]
            then 
                [("ERROR", VError "Match Failed.")]
            else
                res1++res2
        
        ((PTTuple _ p1 p2 p3), (VTTuple v1 v2 v3)) ->
            let
                res1 = match p1 v1

                res2 = match p2 v2

                res3 = match p3 v3
            in
            if res1 == [("ERROR", VError "Match Failed.")] ||
                res2 == [("ERROR", VError "Match Failed.")] ||
                res3 == [("ERROR", VError "Match Failed.")]
            then 
                [("ERROR", VError "Match Failed.")]
            else
                res1++res2++res3

        ((PInt _ n1), (VInt n2)) ->
            if n1 == n2 then [] else [("ERROR", VError "Match Failed.")]

        ((PFloat _ n1), (VFloat n2)) ->
            if n1 == n2 then [] else [("ERROR", VError "Match Failed.")]

        (PTrue _,  VTrue)     -> []
        (PFalse _, VFalse)    -> []

        (PChar _ c1, VChar c2) ->
            if c1 == c2 then [] else [("ERROR", VError "Match Failed.")]
        
        (PNil _, VNil _)    -> []

        (PVar _ s, v) -> [(s, v)]

        _ -> [("ERROR", VError "Match Failed.")]


print : Value -> String
print v  =
    case v of
        VInt n           -> toString n
        VFloat n         -> toString n
        VTrue            -> "true"
        VFalse           -> "false"
        VChar c          -> "\'"++(String.fromChar c)++"\'"
        VNil 0           -> "[]"
        VNil 1           -> "\"\""
        VNil _           -> "Print Value Error: 04."
        VCons 0 v1 v2    -> "[ " ++ (printList v1 v2)

        VCons 1 v1 v2    -> 
            case v1 of
                VChar c ->
                    "\"" ++  (String.fromChar c) ++ (printString v2)
                
                _ ->
                    "Print Value Error: 02."
        
        VCons _ _ _      -> "Print Value Error: 01."
        VError info    -> info
        VClosure _ _ _ -> "<fn>"
        VFix _         -> "<fix>"
        VBTuple v1 v2    -> 
            let
                str1 = print v1
                
                str2 = print v2
            in
            "( "++str1++", "++str2++" )"
        VTTuple v1 v2 v3 ->
            let
                str1 = print v1
                
                str2 = print v2

                str3 = print v3
            in
            "( "++str1++", "++str2++", "++str3++" )"
        VHtml s v1 v2 v3 -> printHTML s v1 v2 v3

printList : Value -> Value -> String
printList v vs =
    case vs of
        VNil 0->
            (print v)++" ]"

        VCons _ v1 v2 -> 
            (print v)++", "++(printList v1 v2)

        _ -> ""

printString : Value -> String
printString v =
    case v of
        VNil 1 ->
            "\""
        
        VCons _ (VChar c) v2 ->
            (String.fromChar c) ++ (printString v2)

        _ ->
            "Print Value Error: 03."


updateElmInVenv : String -> Value -> VEnv -> VEnv
updateElmInVenv s v venv =
    case venv of
        [] -> []

        (s1, v1) :: vv ->
            if s1 == s then
                (s1, v) :: vv
            else
                (s1, v1) :: (updateElmInVenv s v vv)


mergeVEnv : VEnv -> VEnv -> VEnv -> VEnv
mergeVEnv venv1 venv2 venv3 =

    case (venv1, venv2, venv3) of

        ((s1, v1)::env1, (s2, v2)::env2, (_, v3)::env3) ->
            case (v1, v3) of
                (VClosure _ b1 _, VFix (ELam _ _ (ELam _ _ b2))) ->
                    if (b1 /= b2) 
                    then (s1, v1) :: mergeVEnv env1 env2 env3
                    else (s2, v2) :: mergeVEnv env1 env2 env3

                _ ->
                    if (v1 /= v3) 
                    then (s1, v1) :: mergeVEnv env1 env2 env3
                    else (s2, v2) :: mergeVEnv env1 env2 env3
        
        _ -> []


mergeVEnv4 : VEnv -> VEnv -> VEnv -> VEnv -> VEnv
mergeVEnv4 venv1 venv2 venv3 venv4 =

    case ((venv1, venv2, venv3), venv4) of

        (((s1, v1)::env1, (s2, v2)::env2, (s3, v3)::env3), (_, v4)::env4) ->
            case (v1, v4) of
                (VClosure _ b1 _, VFix (ELam _ _ (ELam _ _ b2))) ->
                    if (b1 /= b2) 
                    then (s1, v1) :: mergeVEnv4 env1 env2 env3 env4
                    else (s2, v2) :: mergeVEnv4 env1 env2 env3 env4
                _ ->
                    if (v1 /= v4) then
                        (s1, v1) :: mergeVEnv4 env1 env2 env3 env4
                    else
                        case (v2, v4) of
                            (VClosure _ b1 _, VFix (ELam _ _ (ELam _ _ b2))) ->
                                if (b1 /= b2) 
                                then (s2, v2) :: mergeVEnv4 env1 env2 env3 env4
                                else (s3, v3) :: mergeVEnv4 env1 env2 env3 env4
                            _ ->
                                if (v2 /= v4) then
                                    (s2, v2) :: mergeVEnv4 env1 env2 env3 env4
                                else
                                    (s3, v3):: mergeVEnv4 env1 env2 env3 env4

        _ -> []

patternSubst : VEnv -> Pattern -> Value
patternSubst env p = 
    case p of
        PVar _ s ->
            case findVarByName s env of
                Just val ->
                    val
                
                Nothing  ->
                    VError "Pattern Substitution Error: 01."
        
        PCons (_, id) p1 p2 ->
            if id == psQuo || id == psElm then
                VCons vsId (patternSubst env p1) (patternSubst env p2)
            else
                VCons voId (patternSubst env p1) (patternSubst env p2)

        PBTuple _ p1 p2 ->
            VBTuple (patternSubst env p1) (patternSubst env p2)

        PTTuple _ p1 p2 p3 ->
            VTTuple (patternSubst env p1) 
                    (patternSubst env p2)
                    (patternSubst env p3)
        
        PNil (_, id) -> 
            if id == 3 || id == 4 then
                VNil 1
            else
                VNil 0

        PInt _ n     -> VInt n
        PFloat _ n   -> VFloat n
        PTrue _      -> VTrue
        PFalse _     -> VFalse
        PChar _ c    -> VChar c


matchCase : Value -> Branch -> MatchCaseRes
matchCase v b =
    case b of

        BNSin _ n p e ->
            { venvm  = match p v
            , choice = n
            , ei = e
            , pi = p
            }

        BCom _ b1 b2 ->
            let 
                res = matchCase v b1 
            in
                case res.venvm of
                    [(_, VError _)] ->
                        matchCase v b2
                    
                    _ -> res
        
        _ ->
            { venvm  = []
            , choice = 0
            , ei = EError "Match Case Error."
            , pi = PNil defaultWS
            }


updateBranch : Branch -> Int -> Expr -> Branch
updateBranch branch choice e =
    case branch of
        BNSin ws n p expr ->
            if choice == n
            then BNSin ws n p e
            else BNSin ws n p expr

        BCom ws b1 b2 ->
            BCom ws (updateBranch b1 choice e) (updateBranch b2 choice e)
        
        b -> b


valueToExpr : Value -> Expr
valueToExpr v =
    case v of
        VNil id ->
            case id of
                0 ->
                    ENil defaultWS

                1 ->
                    ENil defaultWS
                
                _ ->
                    EError "VNil Error"
        VInt n ->
            EInt defaultWS n

        VFloat n ->
            EFloat defaultWS n

        VTrue ->
            ETrue defaultWS

        VFalse ->
            EFalse defaultWS

        VChar c ->
            EChar defaultWS c

        VCons _ v1 v2 ->
            let
                e1 =
                    valueToExpr v1

                e2 =
                    valueToExpr v2
            in
                ECons defaultWS e1 e2

        VBTuple v1 v2 ->
            let
                e1 =
                    valueToExpr v1 

                e2 =
                    valueToExpr v2 
            in
                EBTuple defaultWS e1 e2

        VTTuple v1 v2 v3 ->
            let
                e1 =
                    valueToExpr v1 

                e2 =
                    valueToExpr v2 

                e3 =
                    valueToExpr v3 
            in
                ETTuple defaultWS e1 e2 e3

        _ ->
            EError ("Can Not Transfer Value: "++(toString v)++" To Expression.")


findVarByName : String -> VEnv -> Maybe Value
findVarByName s venv =
    case venv of
        [] -> Nothing

        (s1, v1) :: vv ->
            if s1 == s then
                Just v1
            else
                findVarByName s vv


append : Value -> Value -> Value
append l1 l2 =
    case l1 of
        VNil _ ->
            l2

        VCons id1 v1 vs1 ->
            VCons id1 v1 (append vs1 l2)

        _ ->
            VError "List Concat Error."


vlength : Value -> Int
vlength v =
    case v of
        VNil _ ->
            0

        VCons _ _ vs ->
            1 + (vlength vs)
        
        _ ->
            -1


deAppend : Value -> Int -> (Value, Value)
deAppend nl n1 =
    case (nl, n1) of
        (VCons nid _ _, 0) ->
            (VNil nid, nl)
        
        (VCons nid v1 vs, _) ->
            let
                (l1, l2) =
                    deAppend vs (n1 - 1)
            in
                (VCons nid v1 l1, l2)

        (VNil nid, _) ->
            (VNil nid, VNil nid)
        
        _ ->
            ( VError "New Value for Updating Concat Type Error"
            , VError "")


changeWsForList : WS -> Expr -> Expr
changeWsForList ws expr =
    case expr of
        ENil _ ->
            ENil ws

        ECons _ e1 e2 ->
            ECons ws e1 (changeWsForList ws e2)

        _ ->
            EError "Impossible!"


lengthUntil : String -> VEnv -> Int
lengthUntil s venv =
    case venv of
        [] -> -1

        (s1, _) :: vv ->
            if s == s1 then
                0
            else
                1 + (lengthUntil s vv)


printHTML : String -> Value -> Value -> Value -> String
printHTML nodeName style attr childs =
    let

        ststr = printStyle style
        
        st =
            case style of
                VNil 0 ->
                    ""
                _ ->
                    " style=\"" ++ ststr ++ "\""

        at = printAttr attr

        cd = printChilds childs
    in
        "<" ++ nodeName ++ st ++" " ++
        "contenteditable=\"true\" " ++ at ++ ">" ++ cd ++
        "</" ++ nodeName ++ ">"


printStyle : Value -> String
printStyle style =
    case style of
        VCons 0 x (VNil 0) ->
            printProperty x

        VCons 0 x xs ->
            let
                str1 = printProperty x

                str2 = printStyle xs
            in
                str1 ++ str2

        _ ->
            "Print Style Error: 02."


printProperty : Value -> String
printProperty p =
    case p of
        VCons 0 s xs ->
            let
                str1 = printStrNoQuoOrHole s

                str2 = printProValues xs
            in
                str1 ++ ": " ++ str2

        _ ->
            "Print Property Error."


printStrNoQuoOrHole : Value -> String
printStrNoQuoOrHole s =
    case s of
        VCons 1 (VChar c) cs ->
            let
                res = printStrNoQuoOrHole cs
            in
            (String.fromChar c) ++ res

        VNil 1 ->
            ""

        _ ->
            "Print String Without Quotation Marks Error."


printProValues : Value -> String
printProValues ls =
    case ls of
        VCons 0 x (VNil 0) ->
            let
                str = printStrNoQuoOrHole x
            in
                str ++ "; "
        
        VCons 0 x xs ->
            let
                str1 = printStrNoQuoOrHole x

                str2 = printProValues xs
            in
            str1 ++ " " ++ str2
        
        _ ->
            "Print Property Values Error."


printAttr: Value -> String
printAttr attr =
    case attr of
        VCons 0 x (VNil 0) ->
            (printOtherPro x)

        VCons 0 x xs ->
            let
                str1 = printOtherPro x

                str2 = printAttr xs
            in
                str1 ++ " " ++ str2

        VNil 0 ->
            ""
        
        _ ->
            "Print Attributions Error."


printOtherPro : Value -> String
printOtherPro p =
    case p of
        VCons 0 n (VCons 0 v (VNil 0)) ->
            let
                str1 = printStrNoQuoOrHole n

                str2 = printStrNoQuoOrHole v 
            in
                str1 ++ "=\"" ++ str2 ++ "\" "

        _ ->
            "Print Property Error."


printChilds : Value -> String
printChilds childs =
    case childs of
        VNil 0 ->
            ""

        VCons 0 c cs ->
            case c of
                VHtml _ _ _ _ ->
                    let
                        str1 = print c

                        str2 = printChilds cs
                    in
                        str1 ++ str2

                VCons 1 _ _ ->
                    let
                        str1 = printStrNoQuoOrHole c

                        str2 = printChilds cs
                    in
                        str1 ++ str2

                VNil 1 ->
                    let
                        str2 = printChilds cs
                    in
                        str2

                _ ->
                    "Child Type Error."
        
        _ ->
            "Print Childs Error."

appendName : String
appendName = "$append$"
flattenName : String
flattenName = "$flatten$"
mapName : String
mapName = "$map$"
joinName : String
joinName = "$join$"

-- pattern of primitive operation of list
pAppend : Pattern
pAppend = PVar defaultWS appendName
pFlatten : Pattern
pFlatten = PVar defaultWS flattenName
pMap : Pattern
pMap = PVar defaultWS mapName
pJoin : Pattern
pJoin = PVar defaultWS joinName

-- eVar of primitive operation of list
eVarAppend : Expr
eVarAppend = EVar defaultWS appendName
eVarFlatten : Expr
eVarFlatten = EVar defaultWS flattenName
eVarMap : Expr
eVarMap = EVar defaultWS mapName
eVarJoin : Expr
eVarJoin = EVar defaultWS joinName


appendBody : Expr
appendBody = 
    let
        xsStr = "xs"
        ysStr = "ys"
        headStr = "head"
        tailStr = "tail"

        -- Pattern
        pXs = PVar defaultWS xsStr
        pYs = PVar defaultWS ysStr
        pHead = PVar defaultWS headStr
        pTail = PVar defaultWS tailStr
        
        -- EVar
        eVarXs = EVar defaultWS xsStr
        eVarYs = EVar defaultWS ysStr
        eVarHead = EVar defaultWS headStr
        eVarTail = EVar defaultWS tailStr

    in
        ELam    
            defaultWS 
            pXs 
            (
                ELam 
                    defaultWS 
                    pYs 
                    (
                        ECase 
                            defaultWS 
                            eVarXs 
                            (
                                BCom 
                                    defaultWS  
                                    (BSin defaultWS (PNil defaultWS) eVarYs)
                                    (
                                        BSin 
                                            defaultWS 
                                            (PCons defaultWS pHead pTail)
                                            (
                                                ECons
                                                    defaultWS 
                                                    eVarHead
                                                    (
                                                        EApp 
                                                            defaultWS
                                                            (EApp defaultWS eVarAppend eVarTail)
                                                            eVarYs
                                                    )
                                            )
                                    )  
                            )
                    )
            )

flattenBody : Expr
flattenBody = 
    let
        xssStr = "xss"
        xsStr = "xs"
        yssStr = "yss"

        pXss = PVar defaultWS xssStr
        pXs = PVar defaultWS xsStr
        pYss = PVar defaultWS yssStr

        eVarXss = EVar defaultWS xssStr
        eVarXs = EVar defaultWS xsStr
        eVarYss = EVar defaultWS yssStr
    
    in
        ELam 
            defaultWS
            pXss 
            (
                ECase 
                    defaultWS
                    eVarXss
                    (
                        BCom 
                            defaultWS
                            (BSin defaultWS (PNil defaultWS) (ENil ([], eoElm)))
                            (
                                BSin 
                                    defaultWS
                                    (PCons defaultWS pXs pYss)
                                    (
                                        EApp
                                            defaultWS
                                            (EApp defaultWS eVarAppend eVarXs)
                                            (EApp defaultWS eVarFlatten eVarYss)
                                    )
                            )
                    )
            )

mapBody : Expr
mapBody = 
    let
        fStr = "f"
        xsStr = "xs"
        headStr = "head"
        tailStr = "tail"

        pF = PVar defaultWS fStr
        pXs = PVar defaultWS xsStr
        pHead = PVar defaultWS headStr
        pTail = PVar defaultWS tailStr

        eVarF = EVar defaultWS fStr
        eVarXs = EVar defaultWS xsStr
        eVarHead = EVar defaultWS headStr
        eVarTail = EVar defaultWS tailStr

    in
        ELam 
            defaultWS
            pF 
            (
                ELam 
                    defaultWS
                    pXs
                    (
                        ECase  
                            defaultWS
                            eVarXs
                            (
                                BCom
                                    defaultWS
                                    (BSin defaultWS (PNil defaultWS) (ENil ([], eoElm)))
                                    (
                                        BSin
                                            defaultWS
                                            (PCons defaultWS pHead pTail)
                                            (
                                                ECons
                                                    defaultWS
                                                    (EApp defaultWS eVarF eVarHead)
                                                    (
                                                        EApp
                                                            defaultWS
                                                            (EApp defaultWS eVarMap eVarF)
                                                            eVarTail
                                                    )
                                            )
                                    )
                            )
                    )
            )
    

joinBody : Expr
joinBody = 
    let
        sepStr = "sep"
        xsStr = "xs"
        headStr = "head"
        tailStr = "tail"

        pSep = PVar defaultWS sepStr
        pXs = PVar defaultWS xsStr
        pHead = PVar defaultWS headStr
        pTail = PVar defaultWS tailStr

        eVarSep = EVar defaultWS sepStr
        eVarXs = EVar defaultWS xsStr
        eVarHead = EVar defaultWS headStr
        eVarTail = EVar defaultWS tailStr

    in
        ELam 
            defaultWS
            pSep
            (
                ELam 
                    defaultWS
                    pXs
                    (
                        ECase 
                            defaultWS
                            eVarXs 
                            (
                                BCom
                                    defaultWS
                                    (BSin defaultWS (PNil defaultWS) (ENil ([], esElm)))
                                    (
                                        BCom
                                            defaultWS
                                            (
                                                BSin 
                                                    defaultWS 
                                                    (PCons defaultWS pHead (PNil defaultWS))
                                                    eVarHead
                                            )
                                            (
                                                BSin 
                                                    defaultWS
                                                    (PCons defaultWS pHead pTail)
                                                    (
                                                        EBPrim
                                                            defaultWS
                                                            Add
                                                            (EBPrim defaultWS Add eVarHead eVarSep)
                                                            (
                                                                EApp 
                                                                    defaultWS
                                                                    (EApp defaultWS eVarJoin eVarSep)
                                                                    eVarTail
                                                            )
                                                    )
                                            )
                                    )

                            )
                    )
            )


-- preclude primitive operation on list. 
-- append, flatten, map, join,

withPreclude : Expr -> Expr 
withPreclude expr = 
    ELetrec 
        defaultWS
        pAppend
        appendBody
        (
            ELetrec
                defaultWS
                pFlatten
                flattenBody
                (
                    ELetrec
                        defaultWS
                        pMap
                        mapBody
                        (
                            ELetrec
                                defaultWS
                                pJoin
                                joinBody
                                expr
                        )
                )
        )

-- lambda identification for template part
lamTplStr : Expr
lamTplStr =
    ELam
        defaultWS
        pTplStr
        eVarTplStr

lamTplExpr : Expr
lamTplExpr = 
    ELam
        defaultWS
        pTplExpr
        eVarTplExpr

lamTplSet : Expr
lamTplSet = 
    ELam 
        defaultWS
        pTplSet
        eVarTplSet

lamTplIf : Expr
lamTplIf = 
    ELam
        defaultWS
        pTplIf
        eVarTplIf

lamTplForeach : Expr
lamTplForeach = 
    ELam
        defaultWS
        pTplForeach
        eVarTplForeach


-- string constant literal Expr (empty string, CRLF string)
empStrExpr : Expr
empStrExpr = ENil ([" "], esElm)

lfStrExpr : Expr
lfStrExpr = ECons ([" "], esQuo) (EChar ([" "], 0) '\n') (changeWsForList ([], esElm) empStrExpr)

vIdToEId : Int -> Int -> Int
vIdToEId vid originalEId =
    if originalEId == esQuo || originalEId == eoSquare || originalEId == eoAddFromEmp then
        if vid == vsId then
            esQuo
        else if vid == voId then
            eoSquare
        else -1
    else
        if vid == vsId then
            esElm
        else if vid == voId then
            eoElm
        else -1