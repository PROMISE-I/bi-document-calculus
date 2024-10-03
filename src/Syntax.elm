module Syntax exposing (..)

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
tplForeachName = "$tplForeach$"
tplNodeName : String
tplNodeName = "$tplNode$"

treeTplName : String
treeTplName = "$treeTpl$"

specialNodeNameStr : List String
specialNodeNameStr = ["br", "input", "meta", "link"]

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
pTplNode : Pattern
pTplNode = PVar defaultWS tplNodeName

pTreeTpl : Pattern
pTreeTpl = PVar defaultWS treeTplName

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
eVarTplNode : Expr
eVarTplNode = EVar defaultWS tplNodeName

eVarTreeTpl : Expr
eVarTreeTpl = EVar defaultWS treeTplName


type alias Attr = { value : Value }
type alias ExprNode = (ExprChilds, Expr, Attr)

type ExprChilds
    = ECParens ExprNode
    | ECVar
    | ECLam
    | ECApp ExprNode ExprNode ExprNode
    | ECInt 
    | ECFloat 
    | ECTrue 
    | ECFalse  
    | ECChar  
    | ECCons ExprNode ExprNode 
    | ECNil  
    | ECDictDef ECDictPairs
    | ECDictUpd ExprNode ECDictPairs
    | ECField ExprNode 
    | ECBPrim ExprNode ExprNode
    | ECUPrim ExprNode
    | ECCase ExprNode ExprNode
    | ECFix 
    | ECBTuple ExprNode ExprNode
    | ECTTuple ExprNode ExprNode ExprNode
    | ECNode ExprNode ExprNode
    | ECToStr ExprNode
    | ECError

type ECDictPairs 
    = ECNothing
    | ECDictPair ExprNode ECDictPairs


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
    | EDictDef WS EDictPairs
    | EDictUpd WS Expr EDictPairs 
    | EField WS Expr String
    | EBPrim WS Bop Expr Expr
    | EUPrim WS Uop Expr
    | ECase WS Expr Branch
    | EFix WS Expr
    | EBTuple WS Expr Expr
    | ETTuple WS Expr Expr Expr
    | StrTpl Template
    | TreeTpl WS Template
    | ENode WS String Expr Expr
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
    | TplNode WS String Expr Template
    | TplExpr WS Expr
    | TplSet WS Pattern Expr
    | TplIf WS Expr Template Template
    | TplForeach WS Pattern Expr Template
    | TplSplice Expr

type Bop = Add | Sub | Mul | Div | Eq | Lt | Gt | Le | Ge | And | Or | Cat | DDiv
type Uop = Not | Neg

type EDictPairs 
    = ENothing
    | EDictPair WS String Expr EDictPairs

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
    | VDict VDictPairs
    | VFix Expr
    | VClosure Pattern Expr VEnv
    | VBTuple Value Value
    | VTTuple Value Value Value 
    | VNode String Value Value
    | VError Info

type VDictPairs 
    = VNothing
    | VDictPair String Value VDictPairs

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


type DiffOp a
    = DiffInsert a  -- `a` is value to be inserted
    | DiffDelete a  -- `a` is value to be deleted
    | DiffUpdate a  -- `a` is value after update
    | DiffKeep a    -- `a` is value to be kept


type alias MapWalkRes =
    { venv : VEnv
    , fExpr : Expr
    , xsValue : Value
    , diffs : List (DiffOp Value)
    }

type alias FlattenWalkRes =
    { xssValue : Value
    , diffs : List (DiffOp Value)
    }