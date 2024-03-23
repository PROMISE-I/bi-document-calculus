module BDController exposing (..)

import BDModel exposing (..)
import BDSyntax exposing (..)
import BDLangUtils exposing (..)
import BDParser_ exposing (parse)
import Debug exposing (toString)
import BDEval exposing (eval)
import BDDesugar exposing (..)


evalCodeToModel : Code -> Model
evalCodeToModel code =
    let 
        parseResult = 
            parse code 
    in
        case parseResult of
            Result.Ok e ->
                let 
                    desugaredExpr = desugarWithPreclude e
                    e_ = processAfterParse desugaredExpr []
                    res = eval [] e_

                    (output, mode) =
                        case res of
                            VHtml _ _ _ _ ->
                                (print res, HTML)
                            _ ->
                                (print res, Console)

                in
                    { code = code
                    , output = output
                    , codeBackup = code
                    , isOutputChange = False
                    , mode = mode
                    }
            
            Result.Err info ->
                    {code = code
                    , output = toString info
                    , codeBackup = ""
                    , isOutputChange = False
                    , mode = Console
                    }