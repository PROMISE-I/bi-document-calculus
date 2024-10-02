module LangUtils exposing (..)


import Utils exposing (..)
import Syntax exposing (..)
import Debug exposing (toString)
import List exposing (..)

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

                        _ -> "Print Error: 01."
                
                ([], 4) ->
                    case e1 of
                        EChar _ c ->
                            (String.fromChar c) ++ (printAST e2)

                        _ -> "Print Error: 02." ++ Debug.toString expr

                _ ->
                    "Print Error: 03." ++ (Debug.toString (ls, kind))

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
        
        TreeTpl ([ws1, ws2], _) t ->
            "{*" ++ ws1 ++ (printTpl t) ++ "*}" ++ ws2

        ENode ([ws1, ws2], 0) s e1 e2 ->
            "Node" ++ ws1 ++ s ++ ws2 ++ (printAST e1) ++ (printAST e2)

        EToStr ([ws], 0) e ->
            "toString" ++ ws ++ (printAST e)

        EError info ->
            info
        
        _ -> "Print Error: 04." ++ (Debug.toString expr)


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

                        _ -> "Print Error: 05."
                
                ([], 4) ->
                    case p1 of
                        PChar _ c ->
                            (String.fromChar c) ++ (printPattern p2)

                        _ -> "Print Error: 06."

                _ ->
                    "Print Error: 07."
    
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
            "Print Error: 08."


printBranch : Branch -> String
printBranch b =
    case b of
        BSin ([ws], _) p e ->
            (printPattern p) ++ "=>" ++ ws ++ (printAST e)

        BCom ([ws], _) b1 b2 ->
            (printBranch b1) ++ "|" ++ ws ++ (printBranch b2)

        _ ->
            "Print Error: 09."


printTpl : Template -> String
printTpl t =
    case t of
        TCons ctx tplPart restTpl ->
            (printTplPart ctx tplPart) ++ (printTpl restTpl)

        TNil _ -> ""

printTplPart : TplCtx -> TplPart -> String
printTplPart ctx tplPart = 
    if ctx == stctx then
        case tplPart of
            TplStr s -> 
                case s of 
                    -- substitute esQUo(3) -> esElm to avoid print quotes
                    ECons (_, 3) e1 e2 ->
                        printAST (ECons ([], esElm) e1 e2)

                    ENil _ -> ""

                    _ -> "Print Error: 10"

            TplExpr ([ws], _) e -> 
                "{{" ++ ws ++ (printAST e) ++"}}"

            TplSet ([ws1, ws2, ws3], _) p e ->
                "{%" ++ ws1 ++ "set" ++ ws2 ++ (printPattern p) ++ "=" ++ ws3 ++ (printAST e) ++ "%}"

            TplIf ([ws1, ws2, ws3, ws4, ws5, ws6, ws7], _) e t1 t2 ->
                "{%" ++ ws1 ++ "if" ++ ws2 ++ (printAST e) ++ "then" ++ ws3 ++ "%}" ++
                (printTpl t1) ++ 
                "{%" ++ ws4 ++ "else" ++ ws5 ++ "%}" ++
                (printTpl t2) ++ 
                "{%" ++ ws6 ++ "endif" ++ ws7 ++ "%}"
            
            TplForeach ([ws1, ws2, ws3, ws4, ws5], _) p e t -> 
                "{%" ++ ws1 ++ "for" ++ ws2 ++ (printPattern p) ++ "in" ++ ws3 ++ (printAST e) ++ "%}" ++ 
                (printTpl t) ++ 
                "{%" ++ ws4 ++ "endfor" ++ ws5 ++ "%}"

            _ -> "Print Error: 11"
    else
        case tplPart of
            TplStr s -> 
                case s of 
                    -- substitute esQUo(3) -> esElm to avoid print quotes
                    ECons (_, 3) e1 e2 ->
                        printAST (ECons ([], esElm) e1 e2)
                    
                    ENil _ -> ""

                    _ -> "Print Error: 12" ++ Debug.toString s
            
            TplNode ([ws1, ws2], _) n e _ ->    -- for special template node, e.g., <br>
                "<" ++ n ++ ws1 ++ (printTplNodeAttr e) ++ ">" ++ ws2

            TplNode ([ws1, ws2, ws3], _) n e t ->
                "<" ++ n ++ ws1 ++ (printTplNodeAttr e) ++ ">" ++ 
                ws2 ++ (printTpl t) ++ 
                "</" ++ n ++ ">" ++ ws3

            TplExpr ([ws1, ws2], _) e -> 
                "{{" ++ ws1 ++ (printAST e) ++"}}" ++ ws2

            TplSet ([ws1, ws2, ws3, ws4], _) p e ->
                "{%" ++ ws1 ++ "set" ++ ws2 ++ (printPattern p) ++ "=" ++ ws3 ++ (printAST e) ++ "%}" ++ ws4

            TplIf ([ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws10], _) e t1 t2 ->
                "{%" ++ ws1 ++ "if" ++ ws2 ++ (printAST e) ++ "then" ++ ws3 ++ "%}" ++
                ws4 ++ (printTpl t1) ++ 
                "{%" ++ ws5 ++ "else" ++ ws6 ++ "%}" ++
                ws7 ++ (printTpl t2) ++ 
                "{%" ++ ws8 ++ "endif" ++ ws9 ++ "%}" ++ ws10
            
            TplForeach ([ws1, ws2, ws3, ws4, ws5, ws6, ws7], _) p e t -> 
                "{%" ++ ws1 ++ "for" ++ ws2 ++ (printPattern p) ++ "in" ++ ws3 ++ (printAST e) ++ "%}" ++ 
                ws4 ++ (printTpl t) ++ 
                "{%" ++ ws5 ++ "endfor" ++ ws6 ++ "%}" ++ ws7

            _ -> "Print Error: 13" ++ Debug.toString tplPart ++ " "


printTplNodeAttr : Expr -> String
printTplNodeAttr attrs = 
    case attrs of
        ECons _ attr restAttrs ->
            case attr of
                EBTuple ([ws1, ws2, ws3], _) n v ->
                    case n of
                        ECons (_, 3) e1 e2 ->
                            let
                                nWithoutQuo = ECons ([], esElm) e1 e2 
                                vWithoutWs = 
                                    case v of
                                        ECons _ _ _ -> changeWs ([""], esQuo) v
                                        ENil _ -> changeWs ([""], esQuo) v
                                        _ -> v 
                            in
                                (printAST nWithoutQuo) ++ ws1 ++ "=" ++ ws2 ++ (printAST vWithoutWs) ++ ws3 ++ 
                                (printTplNodeAttr restAttrs)

                        _ -> "Print Error: 14"
                
                _ -> "Print Error: 15" ++ (Debug.toString attr)

        ENil _ -> "" 

        _ -> "Print Error: 16"

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

        ENode ws s e1 e2 ->
            let 
                e1_ = processBeforePrint e1 env
            
                e2_ = processBeforePrint e2 env

            in
                ENode ws s e1_ e2_

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

        ENode ws s e1 e2 ->
            let 
                e1_ = processAfterParse e1 env 

                e2_ = processAfterParse e2 env

            in
                ENode ws s e1_ e2_

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
        VNode s v1 v2 -> printNode s v1 v2

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

-- TODO: value merge, recursively traverses the subvalues of the three structurally equivalent values, until the rule for base cases.
mergeVEnv : VEnv -> VEnv -> VEnv -> VEnv 
mergeVEnv venv1 venv2 venv3 =

    case (venv1, venv2, venv3) of

        ((s1, v1)::env1, (s2, v2)::env2, (_, v3)::env3) ->
            case (v1, v3) of
                (VClosure _ b1 _, VFix (ELam _ _ (ELam _ _ b2))) ->
                    -- let
                    --     _ = Debug.log "merge v1" v1
                    --     _ = Debug.log "merge v2" v2
                    --     _ = Debug.log "merge v3" v3
                    --     _ = Debug.log "merge venv3" venv3
                    -- in
                    
                    (s1, v1) :: mergeVEnv env1 env2 env3

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
                    let _ = Debug.log "env" <| s ++ (Debug.toString env) in
                    VError ("Pattern Substitution Error: 01." ++ s)
        
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
                    ENil ([], eoElm)

                1 ->
                    ENil ([], esElm)
                
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

        VCons id v1 v2 ->
            let
                e1 =
                    valueToExpr v1 |> addQuoOrSquareForList

                e2 =
                    valueToExpr v2
            in
                if id == vsId then
                    ECons ([], esElm) e1 e2
                else
                    ECons ([" "], eoElm) e1 e2

        VBTuple v1 v2 ->
            let
                e1 =
                    valueToExpr v1 |> addQuoOrSquareForList

                e2 =
                    valueToExpr v2 |> addQuoOrSquareForList
            in
                EBTuple (["", "", " "], defaultId) e1 e2

        VTTuple v1 v2 v3 ->
            let
                e1 =
                    valueToExpr v1 |> addQuoOrSquareForList 

                e2 =
                    valueToExpr v2 |> addQuoOrSquareForList 

                e3 =
                    valueToExpr v3 |> addQuoOrSquareForList 
            in
                ETTuple (["", "", "", " "], defaultId) e1 e2 e3
        
        VNode n v1 v2 ->
            let
                e1 = 
                    valueToExpr v1 |> addQuoOrSquareForList

                e2 = 
                    valueToExpr v2 |> addQuoOrSquareForList
            in
                ENode ([" ", " "], defaultId) n e1 e2

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
            EError ("Impossible!" ++ (Debug.toString expr))


changeWs : WS -> Expr -> Expr
changeWs ws expr =
    case expr of
        ENil _ -> ENil ws

        ECons _ e1 e2 -> ECons ws e1 e2

        _ -> EError "Change WS Not Implement for this expr pattern!"


lengthUntil : String -> VEnv -> Int
lengthUntil s venv =
    case venv of
        [] -> -1

        (s1, _) :: vv ->
            if s == s1 then
                0
            else
                1 + (lengthUntil s vv)


printNode : String -> Value -> Value -> String
printNode nodeName attr childs = 
    let
        at = printAttr attr
        cd = printChilds childs    
    in
        if List.member nodeName specialNodeNameStr then
            "<" ++ nodeName ++ " " ++ 
            "contenteditable=\"true\" " ++ at ++ ">" 
        else
            "<" ++ nodeName ++ " " ++ 
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
        VBTuple n v ->  
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
                VNode _ _ _ ->
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
                        "<span contenteditable=\"true\">" ++
                        str1 ++ 
                        "</span>" ++
                        str2

                VNil 1 ->
                    let
                        str2 = printChilds cs
                    in
                        str2

                _ ->
                    "Child Type Error." ++ (print c)
        
        _ ->
            "Print Childs Error." ++ Debug.toString childs

appendName : String
appendName = "append"
flattenName : String
flattenName = "flatten"
mapName : String
mapName = "map"
joinName : String
joinName = "join"

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

lamTplNode : Expr
lamTplNode = 
    ELam
        defaultWS
        pTplNode
        eVarTplNode

lamTreeTpl : Expr
lamTreeTpl = 
    ELam 
        defaultWS
        pTreeTpl
        eVarTreeTpl


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


unifyLineSeparator : String -> String
unifyLineSeparator input = 
    let
        replaceRN = String.replace "\r\n" "\n" input
        replaceR = String.replace "\r" "\n" replaceRN

        -- _ = Debug.log "input" <| input
        -- _ = Debug.log "output" <| replaceR

    in
        replaceR


-- 计算两个元素的编辑代价
cost : a -> a -> Int
cost a b =
    if a == b then
        0
    else
        1

-- Diff Operation
generateEditOperations : a -> List a -> List a -> List (DiffOp a)
generateEditOperations default l1 l2 =
    let
        len1 = List.length l1
        len2 = List.length l2
        maxDist = len1 + len2 + 1

        matrix =
            List.range 0 len1
                |> List.foldl
                    (\i row ->
                        (
                            List.range 0 len2
                                |> List.foldl
                                    (\j cell ->
                                        let
                                            -- l1[i-1]
                                            valInList1 = 
                                                case List.head <| List.drop (i-1) l1 of
                                                    Just v -> v
                                                    Nothing -> default
                                            -- l2[j-1]  
                                            valInList2 = 
                                                case List.head <| List.drop (j-1) l2 of
                                                    Just v -> v
                                                    Nothing -> default
                                            errorRes = (-1, []) :: cell
                                        in
                                        
                                            if i == 0 then
                                                if j == 0 then
                                                    (0, []) :: cell
                                                else 
                                                    -- matrix[0][j].second = (DiffInsert l2[j-1]) :: matrix[0][j-1].second
                                                    case List.head cell of
                                                        Just (_, ops) -> (j, (DiffInsert valInList2) :: ops) :: cell  -- add ops && cell in reverse 
                                                        Nothing -> errorRes
                                            else if j == 0 then
                                                -- matrix[i][0].second = (DiffDelete l1[i-1]) :: matrix[i-1][0].second, here we only preverse the former row, thus row is matrix[i-1]
                                                case List.head row of  
                                                    Just (_, ops) -> (i, (DiffDelete valInList1) :: ops) :: cell
                                                    Nothing -> errorRes
                                            else
                                                let
                                                    -- matrix[i][j].first = min(
                                                    --      matrix[i-1][j-1] + (cost l1[i-1] l2[j-1]), 
                                                    --      matrix[i-1][j], 
                                                    --      matrix[j-1]
                                                    --      )

                                                    -- matrix[i-1][j-1]
                                                    (topLeft, topLeftOps) = 
                                                        case List.head <| List.drop (j-1) row of
                                                            Just (v, ops) -> (v, ops)
                                                            Nothing -> (maxDist, [])

                                                    -- matrix[i-1][j]
                                                    (top, topOps) = 
                                                        case List.head <| List.drop j row of
                                                            Just (v, ops) -> (v, ops) 
                                                            Nothing -> (maxDist, [])
                                                    
                                                    -- matrix[i][j-1]
                                                    (left, leftOps) = 
                                                        case List.head cell of
                                                            Just (v, ops) -> (v, ops)
                                                            Nothing -> (maxDist, [])

                                                    substitution = topLeft + cost valInList1 valInList2
                                                    deletion = top + 1
                                                    insertion = left + 1

                                                    minCost =  
                                                        case List.minimum [ substitution, deletion, insertion ] of
                                                            Just v -> v
                                                            Nothing -> maxDist


                                                in
                                                    if minCost == substitution && valInList1 /= valInList2 then
                                                        (minCost, (DiffUpdate valInList2) :: topLeftOps) :: cell
                                                    else if minCost == substitution && valInList1 == valInList2 then
                                                        (minCost, (DiffKeep valInList1) :: topLeftOps) :: cell
                                                    else if minCost == deletion then
                                                        (minCost, (DiffDelete valInList1) :: topOps) :: cell
                                                    else if minCost == insertion then
                                                        (minCost, (DiffInsert valInList2) :: leftOps) :: cell
                                                    else 
                                                        errorRes
                                    )
                                    []
                                |> List.reverse
                        )
                    )
                    []
    in
        case List.head <| List.reverse matrix of
            Just (_, ops) -> List.reverse ops
            Nothing -> []


vConsToList : Value -> List Value
vConsToList v =
    case v of
        VCons _ head tail -> head :: (vConsToList tail)

        VNil _ -> []
        
        _ -> [VError "VCons to list Error: not a list value"]
        

getVConsId : Value -> Int
getVConsId v =
    case v of 
        VNil id -> id

        VCons id _ _ -> id

        _ -> -1


getEConsId : Expr -> Int
getEConsId e =
    case e of
        ENil (_, id) -> id

        ECons (_, id) _ _ -> id

        _ -> -1


getWs : Expr -> WS
getWs e =
    case e of
        ENil ws -> ws

        ECons ws _ _ -> ws

        _ -> defaultWS


listToVCons : List Value -> Int -> Value
listToVCons lst id = 
    case lst of
        [] -> VNil id

        head :: tail -> VCons id head (listToVCons tail id)


stringToECons : WS -> List Char -> Expr
stringToECons ws s =
    case s of
        [] -> ENil ws

        c :: cs -> ECons ws (EChar defaultWS c) (stringToECons ([], esElm) cs)


addQuoOrSquareForList : Expr -> Expr
addQuoOrSquareForList e =
    case e of 
        ECons (_, eid) e1 e2 ->
            if eid == esElm then 
                ECons ([" "], esQuo) e1 e2
            else
                ECons (["", " "], eoSquare) e1 e2
        
        ENil (_, eid) ->
            if eid == esElm then
                ENil ([" "], esQuo)
            else 
                ENil (["", " "], eoSquare)
        
        _ -> e


getExprFromExprNode : ExprNode -> Expr
getExprFromExprNode en =
    case en of
        (_, expr, _) -> expr

getValueFromExprNode : ExprNode -> Value
getValueFromExprNode en =
    case en of
        (_, _, attrs) -> attrs.value


isListValue : Value -> Bool
isListValue v =
    case v of
        VCons _ _ _ -> True
        VNil _ -> True
        _ -> False

calcDiff : Value -> Value -> List (DiffOp Value)
calcDiff oldv newv =
    if isListValue oldv && isListValue newv then
        let
            oldvList = vConsToList oldv
            newvList = vConsToList newv
        in
            generateEditOperations (VError "") oldvList newvList
            
    else 
        []


splitDiffs : List Value -> List Value -> List (DiffOp Value) -> (List (DiffOp Value), List (DiffOp Value))
splitDiffs v1 v2 diffs =
    case v1 of 
        [] -> ([], diffs)
        
        _ :: v1t ->
            case diffs of
                [] -> 
                    if List.isEmpty v2 then
                        ([], [])
                    else 
                        let
                            _ = Debug.log "In splitDiffHelp" "Non Empty list but empty diffs"
                        in
                            ([], [])

                (DiffInsert _ as diff) :: restDiffs ->
                    let
                        (v1Diffs, v2Diffs) = splitDiffs v1 v2 restDiffs
                    in
                        (diff :: v1Diffs, v2Diffs)


                diff :: restDiffs ->
                    let
                        (v1Diffs, v2Diffs) = splitDiffs v1t v2 restDiffs
                    in
                        (diff :: v1Diffs, v2Diffs)


getUpdatedValueInDiff : DiffOp Value -> Value
getUpdatedValueInDiff diff =
    case diff of
        DiffDelete v -> v
        DiffInsert v -> v
        DiffKeep v -> v
        DiffUpdate v -> v

applyDiffs : Value -> List (DiffOp Value) -> Value
applyDiffs oldv diffs =
    case oldv of
        VCons vid _ t ->
            case diffs of
                [] -> VError "Apply Diffs Error: 02 - diffs' length is smaller than the value!"
                
                (DiffDelete _) :: restDiffs -> applyDiffs t restDiffs

                (DiffInsert newv) :: restDiffs -> VCons vid newv (applyDiffs oldv restDiffs)

                diff :: restDiffs -> 
                    let
                        newv = getUpdatedValueInDiff diff
                        newt = applyDiffs t restDiffs
                    in
                        VCons vid newv newt
        
        VNil vid ->
            case diffs of
                [] -> VNil vid

                (DiffInsert v) :: restDiffs -> VCons vid v (applyDiffs (VNil vid) restDiffs)

                _ -> VError ("Apply Diffs Error: 03 - Update/Delete/Keep diff operation on empty list!" ++ " - " ++ Debug.toString diffs)
        
        _ -> VError "Apply Diffs Error: 01 - apply diffs to non list value!"


makeTplIdentity : Expr -> WS -> Expr -> Expr -> Expr -> Expr
makeTplIdentity identity ws oldeTPart neweTPart restTplExpr =
    case neweTPart of
        ENode _ _ attrs _ ->
            let
                ws1 = 
                    if identity == lamTplNode then 
                        alignTpWs (getENodeName oldeTPart) (getENodeName neweTPart) ws
                    else 
                        (["", "", " "], defaultId)
                
                tpWS = 
                    case attrs of
                        ECons _ _ _ -> 
                            case ws1 of
                                ("" :: ts, eid) -> (" " :: ts, eid)
                                _ -> ws1
                        _ -> ws1

            in        
                EApp tpWS lamTplNode (ECons defaultWS neweTPart restTplExpr)
        
        ECons (_, eid) _ _ ->
            if eid == esQuo || eid == esElm then
                EApp defaultWS lamTplStr (ECons defaultWS neweTPart restTplExpr)
            else 
                EError  "Cannot convert a list to a template part!"

        _ -> EError ("Cannot convert expr except node & str to a template part! - " ++ Debug.toString neweTPart)
            
 
vConsTail : Value -> Value
vConsTail v =
    case v of
        VCons _ _ tv -> tv
        _ -> VError "Get tail in VCons Error!"

getVNodeName : Value -> String
getVNodeName v =
    case v of
        VNode n _ _ -> n
        _ -> "Get VNodeName Error."


getENodeName : Expr -> String
getENodeName e = 
    case e of
        ENode _ n _ _ -> n
        _ -> "Get ENodeName Error."


alignTpWs : String -> String -> WS -> WS
alignTpWs oldn newn ws =
    case (List.member oldn specialNodeNameStr, List.member newn specialNodeNameStr) of
        (True, False) ->
            case ws of
                ([ws1, ws2], id) -> ([ws1, "", ws2], id)
                _ -> ws -- error branch
        (False, True) ->
            case ws of
                ([ws1, _, ws3], id) -> ([ws1, ws3], id)
                _ -> ws -- error branch
        _ -> ws


-- TODO: value merge, recursively traverses the subvalues of the three structurally equivalent values, until the rule for base cases.
mergeFunc : Expr -> Expr -> Expr -> Expr
mergeFunc f1 f2 f =
    if f1 /= f then
        f1
    else 
        f2


splitOutputList : Value -> List (DiffOp Value) -> (List (DiffOp Value), List (DiffOp Value))
splitOutputList firstElementValue diffs =
    let
        firstElementList = vConsToList firstElementValue     
        firstElementLen = List.length firstElementList
    in
        case firstElementList of
            [VError _] -> ([], [])

            _ ->
                if firstElementLen < List.length diffs then
                    let
                        maybeNewElementDiffs = List.take firstElementLen diffs 
                        isNewElement = 
                            isAllDelete maybeNewElementDiffs || 
                            isAllKeep maybeNewElementDiffs || 
                            isAllInsert maybeNewElementDiffs
                    in
                        if isNewElement then
                            (maybeNewElementDiffs, List.drop firstElementLen diffs)
                        else
                            -- Caution: the second arg og `splitDiffs` is set to empty, because
                            --          in this version, the second arg is no used in splitDiffs
                            splitDiffs firstElementList [] diffs    

                else 
                    (diffs, [])


isAllDelete : List (DiffOp a) -> Bool
isAllDelete diffs = 
    List.all 
        (\diff -> 
            case diff of
                DiffDelete _ -> True
                _ -> False)
        diffs


isAllKeep : List (DiffOp a) -> Bool
isAllKeep diffs = 
    List.all 
        (\diff -> 
            case diff of
                DiffKeep _ -> True
                _ -> False)
        diffs


isAllInsert : List (DiffOp a) -> Bool
isAllInsert diffs = 
    List.all 
        (\diff -> 
            case diff of
                DiffInsert _ -> True
                _ -> False)
        diffs