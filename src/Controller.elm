module Controller exposing (..)

import Model exposing (..)
import Syntax exposing (..)
import LangUtils exposing (..)
import Parser_ exposing (parse)
import Debug exposing (toString)
import Eval exposing (eval)
import Desugar exposing (..)


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
                    
                    -- lst1 = "kitten" |> String.toList
                    -- lst2 = "sitting" |> String.toList
                    -- _ = Debug.log "lst1" lst1
                    -- _ = Debug.log "lst2" lst2
                    -- _ = Debug.log "diff" <| generateEditOperations '@' lst1 lst2

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
        -- { code = code
        -- , output = Debug.toString parseResult
        -- , codeBackup = code 
        -- , isOutputChange = False
        -- , mode = Console
        -- }