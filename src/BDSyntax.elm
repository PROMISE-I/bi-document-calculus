module BDSyntax exposing (..)

import Html.Attributes exposing (..)
import Parser exposing (..)

type alias Info = String

type alias WS =
    (List String, Int)

defaultWS : WS
defaultWS = ([], 0)
defaultId : number
defaultId = 0

-- VCons: 
vsId : number
vsId = 1    -- String
voId : number
voId = 0    -- Other

-- ECons:
-- ENil like elm
-- Other list begin tag
eoSquare : number
eoSquare = 0
-- Other list element tag
eoElm : number
eoElm = 1
-- Cons operation tag
eoCons : number
eoCons = 2
-- String begin tag
esQuo : number
esQuo = 3
-- String element tag
esElm : number
esElm = 4

-- Single ENil
eoAddFromEmp : number
eoAddFromEmp = 5

-- Single PNil
poNil : number
poNil = 6

-- PCons
poSquare : number
poSquare = 0
poElm : number
poElm = 1
poCons : number
poCons = 2
psQuo : number
psQuo = 3
psElm : number
psElm = 4

-- Case & If
caseN : String
caseN = "$CASE$"
caseId : number
caseId = 1
ifId : number
ifId = 2

-- Template
precludeName : String
precludeName = "$preclude$"
tplStrName : String
tplStrName = "$tplStr$"
tplExprName : String
tplExprName = "$tplExpr$"
tplSetName : String
tplSetName = "$tplSet$"
tplIfName : String
tplIfName = "$tplIf$"
tplForeachName : String
tplForeachName = "$$tplForeach"

pPreclude : Pattern
pPreclude = PVar defaultWS precludeName
pTplStr : Pattern
pTplStr = PVar defaultWS tplStrName
pTplExpr : Pattern
pTplExpr = PVar defaultWS tplExprName
pTplSet : Pattern
pTplSet = PVar defaultWS tplSetName
pTplIf : Pattern
pTplIf = PVar defaultWS tplIfName
pTplForeach : Pattern
pTplForeach = PVar defaultWS tplForeachName

eVarPreclude : Expr
eVarPreclude = EVar defaultWS precludeName
eVarTplStr : Expr
eVarTplStr = EVar defaultWS tplStrName
eVarTplExpr : Expr
eVarTplExpr = EVar defaultWS tplExprName
eVarTplSet : Expr
eVarTplSet = EVar defaultWS tplSetName
eVarTplIf : Expr
eVarTplIf = EVar defaultWS tplIfName
eVarTplForeach : Expr
eVarTplForeach = EVar defaultWS tplForeachName

type Expr
    = EParens WS Expr
    | EVar WS String
    | ELam WS Pattern Expr
    | ELet WS Pattern Expr Expr
    | ELetrec WS Pattern Expr Expr
    | EApp WS Expr Expr
    | EInt WS Int
    | EFloat WS Float
    | ETrue WS
    | EFalse WS
    | EChar WS Char
    | ECons WS Expr Expr
    | ENil WS
    | EBPrim WS Bop Expr Expr
    | EUPrim WS Uop Expr
    | ECase WS Expr Branch
    | EFix WS Expr
    | EBTuple WS Expr Expr
    | ETTuple WS Expr Expr Expr
    | StrTpl Template
    | EHtml WS String Expr Expr Expr
    | EToStr WS Expr
    | EError Info

type Template
    = TCons TplCtx TplPart Template
    | TNil TplCtx

type alias TplCtx = Int

-- string template context & article template context
defaultCtx : TplCtx
defaultCtx = 0
stctx : TplCtx
stctx = 1
atctx : TplCtx
atctx = 2

type TplPart
    = TplStr Expr
    | TplExpr WS Expr
    | TplSet WS Pattern Expr
    | TplIf WS Expr Template Template
    | TplForeach WS Pattern Expr Template
    | TplSplice Expr

type Bop = Add | Sub | Mul | Div | Eq | Lt | Gt | Le | Ge | And | Or | Cat | DDiv
type Uop = Not | Neg

type Branch
    = BSin WS Pattern Expr
    | BNSin WS Int Pattern Expr
    | BCom WS Branch Branch

type Value
    = VInt Int
    | VFloat Float
    | VTrue
    | VFalse
    | VChar Char
    | VCons Int Value Value
    | VNil Int
    | VFix Expr
    | VClosure Pattern Expr VEnv
    | VBTuple Value Value
    | VTTuple Value Value Value 
    | VHtml String Value Value Value
    | VError Info

type alias VEnv = List (String, Value)

type alias IndexedVEnv = List (Int, String, Value)

type Pattern
    = PVar WS String
    | PCons WS Pattern Pattern
    | PNil WS
    | PInt WS Int
    | PFloat WS Float
    | PTrue WS
    | PFalse WS
    | PChar WS Char
    | PBTuple WS Pattern Pattern
    | PTTuple WS Pattern Pattern Pattern


type alias UnEvalRes = 
    { venv : VEnv
    , expr : Expr
    }

type alias MatchCaseRes =
    { venvm : VEnv
    , choice: Int
    , ei : Expr
    , pi : Pattern
    }
