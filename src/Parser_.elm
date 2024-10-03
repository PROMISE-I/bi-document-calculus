module Parser_ exposing (..)

import Set
import Parser exposing (..)
import Syntax exposing (..)
import Parser.Extras exposing (..)
import Parser.Expression exposing (..)
import LangUtils exposing (unifyLineSeparator)
import LangUtils exposing (changeWs)
import Browser.Navigation exposing (back)


mSpaces : Parser String
mSpaces =
    getChompedString (chompWhile isWhiteSpace)


isWhiteSpace : Char -> Bool
isWhiteSpace c =
    c == ' ' || c == '\n' || c == '\r' || c == '\t'


parse : String -> Result (List DeadEnd) Expr
parse =
    run (expr |. end)


node_ : Parser Expr
node_ =
    succeed (\s1 n s2 e1 e2 ->
            ENode ([s1, s2], defaultId) n e1 e2)
    |. symbol "Node"
    |= mSpaces
    |= varName
    |= mSpaces
    |= lazy (\_ -> aexpr)
    |= lazy (\_ -> aexpr)


int_ : Parser Expr
int_ =
    succeed (\n s-> EInt ([s], defaultId) n)
    |= number
        { int = Just identity
        , hex = Just identity
        , octal = Just identity
        , binary = Just identity
        , float = Nothing
        }
    |= mSpaces


float_ : Parser Expr
float_ =
    succeed (\n s -> EFloat ([s], defaultId) n)
    |= number
        { int = Nothing
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just identity
        }
    |= mSpaces


true : Parser Expr
true =
    succeed (\s -> ETrue ([s], defaultId))
        |. keyword "true"
        |= mSpaces


false : Parser Expr
false =
    succeed (\s -> EFalse ([s], defaultId))
        |. keyword "false"
        |= mSpaces


char_ : Parser Char
char_ = 
    map charhelper (getChompedString (chompIf (\_ -> True)))


charhelper : String -> Char
charhelper s =
    case (List.head (String.toList s)) of
        Just c -> c
        Nothing -> ' '


char : Parser Expr
char =
    succeed (\c s -> EChar ([s], defaultId) c)
        |. symbol "\'"
        |= char_
        |. symbol "\'"
        |= mSpaces


string_ : Parser String
string_ = 
    Parser.map unifyLineSeparator (getChompedString (chompUntil "\""))


string : Parser Expr
string =
    succeed (\s ws -> s
                    |> String.toList
                    |> stringToExpr ([ws], esQuo))
        |. symbol "\""
        |= string_
        |. symbol "\""
        |= mSpaces


stringToExpr : WS -> List Char -> Expr
stringToExpr ws s =
    case s of
        [] -> 
            ENil ws
        
        c::cs ->
            ECons ws (EChar defaultWS c) (stringToExpr ([], esElm) cs)


varName : Parser String
varName =
    variable
    { start = \c -> Char.isLower c || c == '_' 
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList <|
            [ "if"
            , "then"
            , "else"
            , "let"
            , "in"
            , "case"
            , "of"
            , "letrec"
            , "nil"
            , "true"
            , "false"
            , "toString"
            -- reserved for template
            , "set"
            , "endif"
            , "for"
            , "endfor"
            ]
    }


var : Parser Expr
var =
    succeed (\v s -> EVar ([s], defaultId) v)
    |= varName
    |= mSpaces


abs : Parser Expr
abs =
    succeed (\s1 p s2 e-> 
            ELam ([s1, s2], defaultId) p e)
        |. symbol "\\"
        |= mSpaces
        |= lazy (\_ -> pattern)
        |. symbol "=>"
        |= mSpaces
        |= lazy (\_ -> expr)


let_ : Parser Expr
let_ =
    succeed (\s1 p s2 e1 s3 e2 -> 
            ELet ([s1, s2, s3], defaultId) p e1 e2)
        |. keyword "let"
        |= mSpaces
        |= lazy (\_ -> pattern)
        |. symbol "="
        |= mSpaces
        |= lazy (\_ -> expr)
        |. keyword "in"
        |= mSpaces
        |= lazy (\_ -> expr)


letrec : Parser Expr
letrec =
    succeed (\s1 p s2 e1 s3 e2 -> 
            ELetrec ([s1, s2, s3], defaultId) p e1 e2)
        |. keyword "letrec"
        |= mSpaces
        |= lazy (\_ -> pattern)
        |. symbol "="
        |= mSpaces
        |= lazy (\_ -> expr)
        |. keyword "in"
        |= mSpaces
        |= lazy (\_ -> expr)


nil : Parser Expr
nil =
    succeed (\s1 s2 ->
            ENil ([s1, s2], eoAddFromEmp))
        |. symbol "["
        |= mSpaces
        |. symbol "]"
        |= mSpaces


caseToApp : Expr -> Branch -> WS -> Expr
caseToApp e b ws =
    EApp ws
        (ELam defaultWS (PVar defaultWS caseN) 
            (ECase defaultWS (EVar defaultWS caseN) b)) e


caseOf : Parser Expr
caseOf =
    succeed (\s1 e s2 b ->
            caseToApp e b ([s1, s2], caseId))
        |. keyword "case"
        |= mSpaces
        |= lazy (\_ -> expr)
        |. keyword "of"
        |= mSpaces
        |= branch


iteToApp : Expr -> Expr -> Expr -> String -> List String -> Expr
iteToApp e1 e2 e3 s1 wsList=
    EApp ([s1], ifId)
        (ELam defaultWS (PVar defaultWS caseN) 
            (ECase (wsList,1) (EVar defaultWS caseN) 
                ( BCom defaultWS
                    (BSin defaultWS (PTrue defaultWS) e2)
                    (BSin defaultWS (PFalse defaultWS) e3)
                ))) e1


iteState : Parser Expr
iteState =
    succeed (\s1 e1 s2 e2 s3 e3 ->
            iteToApp e1 e2 e3 s1 [s2, s3])
        |. keyword "if"
        |= mSpaces
        |= lazy (\_ -> expr)
        |. keyword "then"
        |= mSpaces
        |= lazy (\_ -> expr)
        |. keyword "else"
        |= mSpaces
        |= lazy (\_ -> expr)


sinBranch : Parser Branch
sinBranch = 
    succeed (\p s e ->
            BSin ([s], defaultId) p e)
        |= lazy (\_ -> pattern)
        |. symbol "=>"
        |= mSpaces
        |= lazy (\_ -> expr)


branchOp : OperatorTable Branch
branchOp = [[Infix ( succeed (\s -> BCom ([s], defaultId))
                        |. symbol "|"
                        |= mSpaces
                    ) 
            AssocRight]]


branch : Parser Branch
branch = buildExpressionParser branchOp sinBranch


parens_ : Parser Expr
parens_ =
    succeed (\s1 e s2 -> EParens ([s1, s2], defaultId) e)
        |. symbol "("
        |= mSpaces
        |= lazy (\_ -> expr)
        |. symbol ")"
        |= mSpaces


btuple : Parser Expr
btuple =
    succeed (\s1 e1 s2 e2 s3 ->
                EBTuple ([s1, s2, s3], defaultId) e1 e2)
        |. symbol "("
        |= mSpaces
        |= lazy (\_ -> expr)
        |. symbol ","
        |= mSpaces
        |= lazy (\_ -> expr)
        |. symbol ")"
        |= mSpaces


ttuple : Parser Expr
ttuple =
    succeed (\s1 e1 s2 e2 s3 e3 s4 ->
                ETTuple ([s1, s2, s3, s4], defaultId) e1 e2 e3)
        |. symbol "("
        |= mSpaces
        |= lazy (\_ -> expr)
        |. symbol ","
        |= mSpaces
        |= lazy (\_ -> expr)
        |. symbol ","
        |= mSpaces
        |= lazy (\_ -> expr)
        |. symbol ")"
        |= mSpaces


tostr : Parser Expr
tostr =
    succeed (\s e -> EToStr ([s], defaultId) e)
        |. keyword "toString"
        |= mSpaces
        |= lazy (\_ -> expr)


aexpr : Parser Expr
aexpr =
    oneOf
    [ iteState
    , backtrackable parens_
    , backtrackable btuple
    , ttuple
    , true
    , false
    , backtrackable int_
    , backtrackable float_
    , var
    , lazy (\_ -> abs)
    , lazy (\_ -> let_)
    , lazy (\_ -> letrec)
    , caseOf
    , backtrackable nil
    , list
    , backtrackable dictEmpty
    , backtrackable dictDef
    , dictUpd
    , string
    , char
    , strTpl
    , treeTpl
    , node_
    , tostr
    ]


flip : (a -> b -> c) -> b -> a -> c
flip f x y = f y x

term : Parser Expr
term =
    let
        foldl1 f (x, xs) =
            List.foldl (flip f) x xs
    in
        succeed (foldl1 (EApp defaultWS))
            |=  some (lazy <| \_ -> aexpr)

fieldAccessParser : Parser (Expr -> Expr)
fieldAccessParser =
    succeed (\n s e -> EField ([s], 0) e n)
        |. symbol "."
        |= varName
        |= mSpaces

bopParser : String -> Bop -> Parser (Expr -> Expr -> Expr)
bopParser s op =
    succeed (\ws -> EBPrim ([ws], defaultId) op)
        |. symbol s
        |= mSpaces


uopParser : String -> Uop -> Parser (Expr -> Expr)
uopParser s op =
    succeed (\ws -> EUPrim ([ws], defaultId) op)
        |. symbol s
        |= mSpaces


list : Parser Expr
list =
    succeed (\s1 e1 es s2 ->
        ECons ([s1, s2], eoSquare) e1 es)
        |. symbol "["
        |= mSpaces
        |= lazy (\_ -> expr)
        |= listloop
        |. symbol "]"
        |= mSpaces


listloop : Parser Expr
listloop =
    loop [] listHelper |> (map exprListToECons)


listHelper : List (Expr, WS) -> Parser (Step (List (Expr, WS)) (List (Expr, WS)))
listHelper  revExprs =
    oneOf
    [ succeed (\s e -> Loop ((e, ([s], eoElm)) :: revExprs))
        |. symbol ","
        |= mSpaces
        |= lazy (\_ -> expr)
    , succeed ()
        |> map (\_ -> Done (List.reverse revExprs))
    ]

dictEmpty : Parser Expr
dictEmpty =
    succeed (\s1 s2 -> (EDictDef ([s1, s2], 0) ENothing))
        |. symbol "{"
        |= mSpaces
        |. symbol "}"
        |= mSpaces

dictDef : Parser Expr
dictDef = 
    succeed (
        \s1 name s2 s3 e dpsLst s4 -> 
            let
                dps = dictPairsListToEDictPairs (((["", s2, s3], 0), name, e) :: dpsLst)
            in
                EDictDef ([s1, s4], 0) dps
    )
        |. symbol "{"
        |= mSpaces 
        |= varName
        |= mSpaces 
        |. symbol "="
        |= mSpaces
        |= expr
        |= dictPairs
        |. symbol "}"
        |= mSpaces


dictUpd : Parser Expr
dictUpd =
    succeed (
        \s1 originDict s2 name s3 s4 e dpsLst s5 ->
            let
                dps = dictPairsListToEDictPairs (((["", s3, s4], 0), name, e)::dpsLst)
            in
                EDictUpd ([s1, s2, s5], 0) originDict dps
    )
        |. symbol "{"
        |= mSpaces 
        |= expr
        |. symbol "|"
        |= mSpaces
        |= varName
        |= mSpaces 
        |. symbol "="
        |= mSpaces
        |= expr
        |= dictPairs
        |. symbol "}"
        |= mSpaces

dictPairs : Parser (List (WS, String, Expr))
dictPairs =
    loop [] dictPairsHelper


dictPairsHelper : List (WS, String, Expr) -> Parser (Step (List (WS, String, Expr)) (List (WS, String, Expr)))
dictPairsHelper revDictPairs =
    oneOf
    [ succeed (\s1 name s2 s3 e -> Loop ((([s1, s2, s3], 0), name, e) :: revDictPairs))
        |. symbol ","
        |= mSpaces
        |= varName
        |= mSpaces
        |. symbol "="
        |= mSpaces 
        |= lazy (\_ -> expr)
    , succeed ()
        |> map (\_ -> Done (List.reverse revDictPairs))
    ]


exprListToECons : List (Expr, WS) -> Expr
exprListToECons ls =
    case ls of
        [] ->
            ENil ([], eoElm)
        (e, s) :: es ->
            ECons s e (exprListToECons es)


cons : Parser (Expr -> Expr -> Expr)
cons =
    succeed (\s -> ECons ([s], eoCons))
        |. symbol "::"
        |= mSpaces


-- Expression Parser for Template
strTpl : Parser Expr
strTpl = 
    succeed (\tb -> StrTpl tb)
        |. symbol "{#"
        |= tplBody stctx
        |. symbol "#}"
        -- TODO add mspaces in the end of strtpl

treeTpl : Parser Expr
treeTpl = 
    succeed (\s1 tb s2 -> TreeTpl ([s1, s2], defaultId) tb)
        |. symbol "{*"
        |= mSpaces
        |= tplBody atctx
        |. symbol "*}"
        |= mSpaces

tplBody : TplCtx -> Parser Template
tplBody ctx = 
    Parser.map
        (tplPartsToTemplate ctx)
        (loop [] <| tplBodyHelp ctx)


tplBodyHelp : TplCtx -> List TplPart -> Parser (Step (List TplPart) (List TplPart))
tplBodyHelp ctx revTplParts = 
    oneOf
        [ succeed (\tp -> Loop (tp :: revTplParts))
            |= tplPart ctx
        , succeed ()
            |> map (\_ -> Done (List.reverse revTplParts))
        ]

tplPartsToTemplate : TplCtx -> List TplPart -> Template
tplPartsToTemplate ctx tps =
    case tps of
        [] -> TNil ctx
        tp :: rest -> TCons ctx tp (tplPartsToTemplate ctx rest)


tplPart : TplCtx -> Parser TplPart
tplPart ctx = 
    let
        strTplPartParsers = 
            [ tplExpr ctx
            , tplSet ctx 
            , tplIf ctx
            , tplForeach ctx
            , tplStr ctx
            ]  
        
        treeTplPartParsers = 
            List.append strTplPartParsers [tplSpecialNode, tplNode]

        tplPartParsers = 
            if ctx == stctx then
                strTplPartParsers
            else 
                treeTplPartParsers
    in
        oneOf tplPartParsers


-- stop words for string template part in string template
strTplStrStops : List String
strTplStrStops = ["{{", "{%", "#}"]

-- stop words for text template part in tree template
treeTplTextStops : List String
treeTplTextStops = ["<", "{{", "{%", "*}"]


stopParser : List String -> Parser ()
stopParser stops = 
    oneOf (List.map (\stop -> symbol stop) stops)

tplStrChompCallBack : List String -> Int -> Result (List Parser.DeadEnd) () -> Parser ()
tplStrChompCallBack stops acc result =
    case result of
        Ok _ -> 
            -- meet stop words
            case acc of
                0 -> Parser.problem ""
                _ -> Parser.succeed()
            
        Err _ -> 
            -- chomp one char, and continue check is stop
                succeed ()
                    |. chompIf (\_ -> True)
                    |. runInnerParser (stopParser stops) (tplStrChompCallBack stops (acc + 1))

tplStr : TplCtx -> Parser TplPart
tplStr ctx = 
    let
        stops = 
            if ctx == stctx then 
                strTplStrStops 
            else 
                treeTplTextStops
    in
        succeed (\s -> s 
                    |> unifyLineSeparator
                    |> String.toList
                    |> stringToExpr ([""], esQuo)
                    |> TplStr)
            |=  getChompedStringUntilAny stops

getChompedStringUntilAny : List String -> Parser String
getChompedStringUntilAny stops = 
    runInnerParser (stopParser stops) (tplStrChompCallBack stops 0) 
        |> getChompedString


specialNodeName : Parser String
specialNodeName = 
    oneOf <| List.map (\s -> succeed(s) |. symbol s) specialNodeNameStr 

tplSpecialNode : Parser TplPart
tplSpecialNode =
    succeed (\n s1 attrs s2 -> 
                TplNode 
                    ([s1, s2], defaultId)
                    n
                    attrs
                    (TNil atctx)
            )
        |. backtrackable (symbol "<")
        |= specialNodeName
        |= mSpaces
        |= tplNodeAttrs
        |. symbol ">"
        |= mSpaces

tplNode : Parser TplPart
tplNode = 
    -- <<n1><s1><attrs>> <s2><t> </<n2>> <s3>
    succeed (\n1 s1 attrs s2 t n2 s3 -> -- TODO: assert n1 == n2
                TplNode 
                    ([s1, s2, s3], defaultId) 
                    n1
                    attrs
                    t
            ) 
        |. backtrackable (symbol "<")
        |= varName
        |= mSpaces
        |= tplNodeAttrs
        |. symbol ">"
        |= mSpaces
        |= tplBody atctx
        |. symbol "</"
        |= varName
        |. symbol ">"
        |= mSpaces


tplNodeAttrs : Parser Expr
tplNodeAttrs = 
    loop [] tplNodeAttrsHelp 
        |> map exprListToECons 
        |> map (changeWs ([" ", " "], eoSquare))

tplNodeAttrsHelp : List (Expr, WS) -> Parser (Step (List (Expr, WS)) (List (Expr, WS)))
tplNodeAttrsHelp revAttrs =
    oneOf 
        [ succeed (\attr -> Loop ((attr, ([" "], eoElm)) :: revAttrs))
                |= tplNodeAttr
            , succeed ()
                |> map (\_ -> Done (List.reverse revAttrs))
        ]

tplNodeAttr : Parser Expr
tplNodeAttr = 
    -- <n><s1>=<s2><e>
    succeed (\n s1 s2 e -> 
                let
                    (v1, s3) = 
                        case e of
                            ECons ([ws], eid) _ _ -> (changeWs ([""], eid) e, ws)
                            _ -> (e, "")
                in
                    EBTuple 
                        ([s1, s2, s3], defaultId) 
                        (n |> String.toList |> stringToExpr ([" "], esQuo))
                        v1
            )
        |= varName
        |= mSpaces
        |. symbol "="
        |= mSpaces
        |= aexpr


tplExpr : TplCtx -> Parser TplPart
tplExpr ctx = 
    if ctx == stctx then
        -- {{<s><e>}}
        succeed (\s e -> TplExpr ([s], defaultId) e)
            |. symbol "{{"
            |= mSpaces
            |= lazy (\_ -> expr)
            |. symbol "}}"
    else
        -- {{<s1><e>}}<s2>
        succeed (\s1 e s2 -> TplExpr ([s1, s2], defaultId) e)
            |. symbol "{{"
            |= mSpaces
            |= lazy (\_ -> expr)
            |. symbol "}}"
            |= mSpaces


tplSet : TplCtx -> Parser TplPart
tplSet ctx = 
    if ctx == stctx then
        -- {%<s1>set<s2><p>=<s3><e>%}
        succeed (\s1 s2 p s3 e -> 
                    TplSet ([s1, s2, s3], defaultId) p e)
            |. backtrackable (symbol "{%")
            |= backtrackable mSpaces
            |. symbol "set"
            |= mSpaces
            |= lazy (\_ -> pattern)
            |. symbol "="
            |= mSpaces
            |= lazy (\_ -> expr)
            |. symbol "%}"
    else
        -- {%<s1>set<s2><p>=<s3><e>%}<s4>
            succeed (\s1 s2 p s3 e s4 -> 
                    TplSet ([s1, s2, s3, s4], defaultId) p e)
            |. backtrackable (symbol "{%")
            |= backtrackable mSpaces
            |. symbol "set"
            |= mSpaces
            |= lazy (\_ -> pattern)
            |. symbol "="
            |= mSpaces
            |= lazy (\_ -> expr)
            |. symbol "%}"
            |= mSpaces


tplIf : TplCtx -> Parser TplPart
tplIf ctx =
    succeed (\((ss1, _), e, tIf) 
              ((ss2, _), tElse)
              (ss3, _) ->
                TplIf 
                    (ss1 ++ ss2 ++ ss3, defaultId) 
                    e 
                    tIf
                    tElse
            )
        |= tplIfBody ctx
        |= tplElseBody ctx
        |= tplIfEnd ctx


tplIfBody : TplCtx -> Parser (WS, Expr, Template)
tplIfBody ctx = 
    if ctx == stctx then
        -- {%<s1>if<s2><expr>then<s3>%}<t>
        succeed (\s1 s2 e s3 t -> 
                    (([s1, s2, s3], defaultId), e, t)
                )
            |. backtrackable (symbol "{%")
            |= backtrackable mSpaces
            |. symbol "if"
            |= mSpaces 
            |= lazy (\_ -> expr)
            |. symbol "then"
            |= mSpaces
            |. symbol "%}"
            |= lazy (\_ -> tplBody ctx)
    else 
        -- {%<s1>if<s2><expr>then<s3>%}<s4><t>
        succeed (\s1 s2 e s3 s4 t -> 
                    (([s1, s2, s3, s4], defaultId), e, t)
                )
            |. backtrackable (symbol "{%")
            |= backtrackable mSpaces
            |. symbol "if"
            |= mSpaces 
            |= lazy (\_ -> expr)
            |. symbol "then"
            |= mSpaces
            |. symbol "%}"
            |= mSpaces
            |= lazy (\_ -> tplBody ctx)

tplElseBody : TplCtx -> Parser (WS, Template)
tplElseBody ctx = 
    if ctx == stctx then
        -- {%<s1>else<s2>%}<t>
        succeed (\s1 s2 t -> 
                    (([s1, s2], defaultId), t)
                )
            |. symbol "{%"
            |= mSpaces
            |. symbol "else"
            |= mSpaces
            |. symbol "%}"
            |= lazy (\_ -> tplBody ctx)
    else
        -- {%<s1>else<s2>%}<s3><t>
        succeed (\s1 s2 s3 t -> 
                    (([s1, s2, s3], defaultId), t)
                )
            |. symbol "{%"
            |= mSpaces
            |. symbol "else"
            |= mSpaces
            |. symbol "%}"
            |= mSpaces
            |= lazy (\_ -> tplBody ctx)

tplIfEnd : TplCtx -> Parser WS
tplIfEnd ctx = 
    if ctx == stctx then
        -- {%<s1>endif<s2>%}
        succeed (\s1 s2 -> 
                    ([s1, s2], defaultId)
                )
            |. symbol "{%"
            |= mSpaces
            |. symbol "endif"
            |= mSpaces
            |. symbol "%}"
    else 
        -- {%<s1>endif<s2>%}<s3>
        succeed (\s1 s2 s3 -> 
                    ([s1, s2, s3], defaultId)
                )
            |. symbol "{%"
            |= mSpaces
            |. symbol "endif"
            |= mSpaces
            |. symbol "%}"
            |= mSpaces


tplForeach : TplCtx -> Parser TplPart
tplForeach ctx = 
    succeed (\((ss1, _), (p, e, t)) (ss2, _) -> 
                TplForeach 
                    (ss1 ++ ss2, defaultId) 
                    p 
                    e 
                    t
            )
        |= tplForeachBody ctx
        |= tplForeachEnd ctx

tplForeachBody : TplCtx -> Parser (WS, (Pattern, Expr, Template))
tplForeachBody ctx =    
    if ctx == stctx then
        -- {%<s1>for<s2><p>in<s3><e>%}<t>
        succeed (\s1 s2 p s3 e t -> 
                    (([s1, s2, s3], defaultId), (p, e, t))
                )
            |. backtrackable (symbol "{%")
            |= backtrackable mSpaces
            |. symbol "for"
            |= mSpaces
            |= lazy (\_ -> pattern)
            |. symbol "in"
            |= mSpaces
            |= lazy (\_ -> expr)
            |. symbol "%}"
            |= lazy (\_ -> tplBody ctx)
    else
        -- {%<s1>for<s2><p>in<s3><e>%}<s4><t>
        succeed (\s1 s2 p s3 e s4 t -> 
                    (([s1, s2, s3, s4], defaultId), (p, e, t))
                )
            |. backtrackable (symbol "{%")
            |= backtrackable mSpaces
            |. symbol "for"
            |= mSpaces
            |= lazy (\_ -> pattern)
            |. symbol "in"
            |= mSpaces
            |= lazy (\_ -> expr)
            |. symbol "%}"
            |= mSpaces
            |= lazy (\_ -> tplBody ctx)

tplForeachEnd : TplCtx -> Parser WS
tplForeachEnd ctx =
    if ctx == stctx then
        -- {%<s1>endfor<s2>%}
        succeed (\s1 s2 ->
                    ([s1, s2], defaultId)
                )
            |. symbol "{%"
            |= mSpaces
            |. symbol "endfor"
            |= mSpaces
            |. symbol "%}"
    else 
        -- {%<s1>endfor<s2>%}<s3>
        succeed (\s1 s2 s3 ->
                    ([s1, s2, s3], defaultId)
                )
            |. symbol "{%"
            |= mSpaces
            |. symbol "endfor"
            |= mSpaces
            |. symbol "%}"
            |= mSpaces


operators : OperatorTable Expr
operators =
    [ [Prefix (uopParser "-" Neg)]
    , [Prefix (uopParser "!" Not)]
    , [Infix cons AssocRight]
    , [Infix (bopParser "*" Mul) AssocLeft, Infix (bopParser "//" Div) AssocLeft]
    , [Infix (backtrackable (bopParser "+" Add)) AssocLeft, Infix (bopParser "-" Sub) AssocLeft]
    , [Infix (backtrackable (bopParser "/" DDiv)) AssocLeft]
    , [Infix (bopParser "++" Cat) AssocLeft]
    , [ Infix (backtrackable (bopParser "<" Lt)) AssocNone
    , Infix (backtrackable (bopParser ">" Gt)) AssocNone]
    , [Infix (bopParser "<=" Le) AssocNone
    , Infix (bopParser ">=" Ge) AssocNone]
    
    , [Infix (bopParser "==" Eq) AssocNone]
    , [Infix (bopParser "&&" And) AssocLeft]
    , [Infix (bopParser "||" Or) AssocLeft]

    , [Postfix fieldAccessParser]
    ]


expr : Parser Expr
expr =
    buildExpressionParser operators (lazy <| \_ -> term)


pvar : Parser Pattern
pvar =
    succeed (\v s -> PVar ([s], defaultId) v)
        |= varName
        |= mSpaces


pnil : Parser Pattern
pnil =
    succeed (\s1 s2 ->
            PNil ([s1, s2], poNil))
        |. symbol "["
        |= mSpaces
        |. symbol "]"
        |= mSpaces


pint : Parser Pattern
pint =
    oneOf 
        [ succeed (\n s -> PInt ([s], defaultId) n)
            |= number
                { int = Just identity
                , hex = Just identity 
                , octal = Just identity
                , binary = Just identity 
                , float = Nothing
                }
            |= mSpaces
        , succeed (\n s -> PInt ([s], defaultId) (negate n))
            |. symbol "-"
            |= int
            |= mSpaces
        ]


pfloat : Parser Pattern
pfloat =
    oneOf
        [ succeed (\n s -> PFloat ([s], defaultId) n)
            |= number
                { int = Nothing
                , hex = Nothing 
                , octal = Nothing
                , binary = Nothing 
                , float = Just identity
                }
            |= mSpaces
        , succeed (\n s -> PFloat ([s], defaultId) (negate n))
            |. symbol "-"
            |= float
            |= mSpaces
        ]


ptrue : Parser Pattern
ptrue =
    succeed (\s -> PTrue ([s], defaultId))
        |. keyword "true"
        |= mSpaces


pfalse : Parser Pattern
pfalse =
    succeed (\s -> PFalse ([s], defaultId))
        |. keyword "false"
        |= mSpaces


pchar : Parser Pattern
pchar =
    succeed (\c s -> PChar ([s], defaultId) c)
        |. symbol "\'"
        |= char_
        |. symbol "\'"
        |= mSpaces


pstring : Parser Pattern
pstring =
    succeed (\s ws -> s
                    |> String.toList
                    |> stringToPattern ([ws], psQuo))
        |. symbol "\""
        |= string_
        |. symbol "\""
        |= mSpaces


stringToPattern : WS -> List Char -> Pattern
stringToPattern ws s =
    case s of
        [] -> 
            PNil ws
        
        c::cs ->
            PCons ws (PChar defaultWS c) (stringToPattern ([],psElm) cs)


pList : Parser Pattern
pList =
    succeed (\s1 p ps s2->
            PCons ([s1, s2], poSquare) p ps)
        |. symbol "["
        |= mSpaces
        |= lazy (\_ -> pterm)
        |= pListloop
        |. symbol "]"
        |= mSpaces


pListloop : Parser Pattern
pListloop =
    loop [] pListHelper |> (map ptermListToPCons)


pListHelper : List (Pattern, WS) -> Parser (Step (List (Pattern, WS)) (List (Pattern, WS)))
pListHelper revPats =
    oneOf
    [ succeed (\s p -> Loop ((p, ([s], poElm)) :: revPats))
        |. symbol ","
        |= mSpaces
        |= lazy (\_ -> pterm)
    , succeed ()
        |> map (\_ -> Done (List.reverse revPats))
    ]


ptermListToPCons : List (Pattern, WS) -> Pattern
ptermListToPCons ls =
    case ls of
        [] ->
            PNil ([], poElm)
        (p, s) :: ps ->
            PCons s p (ptermListToPCons ps)


pbtuple : Parser Pattern
pbtuple =
    succeed (\s1 p1 s2 p2 s3 ->
                PBTuple ([s1, s2, s3], defaultId) p1 p2)
        |. symbol "("
        |= mSpaces
        |= lazy (\_ -> pattern)
        |. symbol ","
        |= mSpaces
        |= lazy (\_ -> pattern)
        |. symbol ")"
        |= mSpaces


pttuple : Parser Pattern
pttuple =
    succeed (\s1 p1 s2 p2 s3 p3 s4 ->
                PTTuple ([s1, s2, s3, s4], defaultId) p1 p2 p3)
        |. symbol "("
        |= mSpaces
        |= lazy (\_ -> pattern)
        |. symbol ","
        |= mSpaces
        |= lazy (\_ -> pattern)
        |. symbol ","
        |= mSpaces
        |= lazy (\_ -> pattern)
        |. symbol ")"
        |= mSpaces


pterm : Parser Pattern
pterm =
    oneOf
    [ pvar
    , backtrackable pnil
    , lazy (\_ -> pList)
    , backtrackable pint
    , backtrackable pfloat
    , ptrue
    , pfalse
    , backtrackable pbtuple
    , pttuple
    , pstring
    , pchar
    ]


pConsOp : OperatorTable Pattern
pConsOp = [[Infix ( succeed (\s -> PCons ([s], poCons))
                        |. symbol "::"
                        |= mSpaces) 
            AssocRight]]


pattern : Parser Pattern
pattern = buildExpressionParser pConsOp (lazy (\_ -> pterm))


parseVal : String -> List Value -> Result (List DeadEnd) Value
parseVal s context =
    run (value context |. end) s


value : List Value -> Parser Value
value context =
    oneOf
    [ backtrackable vInt
    , backtrackable vFloat
    , vTrue
    , vFalse
    , backtrackable vNil
    , lazy (\_ -> (vList context))
    , backtrackable <| vDictEmpty context
    , backtrackable <| vDict context
    , backtrackable (vBtuple context)
    , vTtuple context
    , vChar
    , vString
    ]


vBtuple : List Value -> Parser Value
vBtuple context =
    succeed VBTuple
        |. symbol "("
        |. spaces
        |= lazy (\_ -> value context)
        |. symbol ","
        |. spaces
        |= lazy (\_ -> value context)
        |. symbol ")"
        |. spaces


vTtuple : List Value -> Parser Value
vTtuple context =
    succeed VTTuple
        |. symbol "("
        |. spaces
        |= lazy (\_ -> value context)
        |. symbol ","
        |. spaces
        |= lazy (\_ -> value context)
        |. symbol ","
        |. spaces
        |= lazy (\_ -> value context)
        |. symbol ")"
        |. spaces


vList : List Value -> Parser Value
vList context =
    succeed (VCons voId)
        |. symbol "["
        |. spaces
        |= lazy (\_ -> value context)
        |= vListloop context
        |. symbol "]"
        |. spaces


vListloop : List Value -> Parser Value
vListloop context =
    vListHelper context 
    |> loop []
    |> (map valueListToVCons)


vListHelper : List Value -> List Value -> Parser (Step (List Value) (List Value))
vListHelper context revValues =
    oneOf
    [ succeed (\e -> Loop (e :: revValues))
        |. symbol ","
        |. spaces
        |= lazy (\_ -> value context)
    , succeed ()
        |> map (\_ -> Done (List.reverse revValues))
    ]

vDictEmpty : List Value -> Parser Value
vDictEmpty context =
    succeed (VDict VNothing)
        |. symbol "{"
        |. spaces
        |. symbol "}"
        |. spaces

vDict : List Value -> Parser Value
vDict context = 
    succeed (
        \name e dpsLst -> 
            let
                dps = vDictPairsListToVDictPairs ((name, e) :: dpsLst)
            in
                VDict dps
    )
        |. symbol "{"
        |. spaces 
        |= varName
        |. spaces 
        |. symbol "="
        |. spaces
        |= lazy (\_ -> value context)
        |= vDictPairs context
        |. symbol "}"
        |. spaces

vDictPairs : List Value -> Parser (List (String, Value))
vDictPairs context =
    loop [] <| vDictPairsHelper context 


vDictPairsHelper : List Value -> List (String, Value) -> Parser (Step (List (String, Value)) (List (String, Value)))
vDictPairsHelper context revDictPairs =
    oneOf
    [ succeed (\name e -> Loop ((name, e) :: revDictPairs))
        |. symbol ","
        |. spaces
        |= varName
        |. spaces
        |. symbol "="
        |. spaces 
        |= lazy (\_ -> value context)
    , succeed ()
        |> map (\_ -> Done (List.reverse revDictPairs))
    ]

valueListToVCons : List Value -> Value
valueListToVCons ls =
    case ls of
        [] ->
            VNil voId
        v :: vs ->
            VCons voId v (valueListToVCons vs)


vNil : Parser Value
vNil =
    succeed (VNil voId)
        |. symbol "["
        |. spaces
        |. symbol "]"
        |. spaces


vInt : Parser Value
vInt =
    oneOf
        [ succeed VInt
            |= number
                { int = Just identity
                , hex = Just identity
                , octal = Just identity
                , binary = Just identity
                , float = Nothing
                }
            |. spaces
        , succeed (\n -> VInt (negate n)) 
            |. symbol "-"
            |= int
            |. spaces
        ]


vFloat : Parser Value
vFloat =
    oneOf
        [ succeed VFloat
            |= number
                { int = Nothing
                , hex = Nothing
                , octal = Nothing
                , binary = Nothing
                , float = Just identity
                }
            |. spaces
        , succeed (\n -> VFloat (negate n))
            |. symbol "-"
            |= float
            |. spaces
        ]


vTrue : Parser Value
vTrue =
    succeed VTrue
        |. keyword "true"
        |. spaces


vFalse : Parser Value
vFalse =
    succeed VFalse
        |. keyword "false"
        |. spaces


vChar : Parser Value
vChar =
    succeed (\c -> VChar  c)
        |. symbol "\'"
        |= char_
        |. symbol "\'"
        |. spaces


vString : Parser Value
vString =
    succeed (\s -> s
                    |> String.toList
                    |> stringToValue)
        |. symbol "\""
        |= string_
        |. symbol "\""
        |. spaces


stringToValue : List Char -> Value
stringToValue s =
    case s of
        [] -> 
            VNil vsId
        
        c::cs ->
            VCons vsId (VChar c) (stringToValue cs)

dictPairsListToEDictPairs : List (WS, String, Expr) -> EDictPairs
dictPairsListToEDictPairs lst =
    case lst of
        [] -> ENothing
        (ws, name, vexpr) :: rest ->
            EDictPair ws name vexpr (dictPairsListToEDictPairs rest)

vDictPairsListToVDictPairs : List (String, Value) -> VDictPairs
vDictPairsListToVDictPairs lst =
    case lst of
        [] -> VNothing
        (n, v) :: rest ->
            VDictPair n v (vDictPairsListToVDictPairs rest)

-- For Debug

log : String -> Parser a -> Parser a
log message parser =
    succeed ()
        |> andThen
            (\() ->
                let
                    _ =
                        Debug.log "starting" message
                in
                succeed
                    (\source offsetBefore parseResult offsetAfter ->
                        let
                            _ =
                                Debug.log "-----------------------------------------------" message

                            _ =
                                Debug.log "source         " source
                            
                            _ = Debug.log "offset before  " offsetBefore

                            _ =
                                Debug.log "chomped string " (String.slice offsetBefore offsetAfter source)

                            _ =
                                Debug.log "parsed result  " parseResult
                        in
                        parseResult
                    )
                    |= getSource
                    |= getOffset
                    |= parser
                    |= getOffset
            )


runInnerParser : Parser a -> (Result (List Parser.DeadEnd) a -> Parser b) -> Parser b
runInnerParser parser callback = 
    Parser.getSource 
        |> Parser.andThen
            (
                \source -> 
                    Parser.getOffset
                        |> Parser.andThen
                            (
                                \offset -> 
                                    Parser.run parser (String.dropLeft offset source)
                                        |> callback
                            )
            )
