module BDHtmlParser exposing (..)

import List exposing (map, foldr)
import BDSyntax exposing (Value(..))
import Html.Parser exposing (run, Node(..), Attribute)
import String exposing (toList, trim, fromList, split, left, dropRight)

parseHtml : String -> Value
parseHtml s =
    let
        parseRes =
            run s
    in
    case parseRes of
        Result.Ok res ->
            case res of
                node :: [] ->
                    nodeToValue node

                _ ->
                    VError "There cannot be two or more or 0 root nodes."

        Result.Err _ ->
            VError "Parse Html Error."


nodeToValue : Node -> Value
nodeToValue node =
        case node of
            Element s attrList childs ->
                case s of
                    "span" ->
                        case childs of
                            [Text c] ->
                                trim c |> toList |> stringToVCons

                            _ ->
                                VError "Parse Hole Error: 01."
                    _ ->
                        let
                            vChilds =
                                parseChilds childs
                        in
                        case attrList of
                            [] ->
                                VHtml s (VNil 0) (VNil 0) vChilds

                            ("style", pro) :: [] ->
                                VHtml s (pro    
                                            |> split ";"
                                            |> parseStyle) 
                                    (VNil 0) vChilds
                            
                            ("style", pro) :: al ->
                                VHtml s (pro
                                        |> split ";"
                                        |> parseStyle) 
                                    (parseOtherPro al) vChilds

                            al -> 
                                VHtml s (VNil 0)
                                (parseOtherPro al) vChilds

            Text s ->
                stringToVCons <| toList s
            
            _ ->
                VError "An error occurred in the node constructor."


parseChilds : List Node -> Value
parseChilds childs =
    case childs of
        [] -> VNil 0

        c :: cds ->
            VCons 0 (nodeToValue c) (parseChilds cds)


parseStyle : List String -> Value
parseStyle pro =
    case pro of
        s :: ps ->
            if (trim s |> left 1) == "{" then
                s |> trim |> toList |> stringToVCons
            else
                let
                    nameAndValue =
                        split ":" <| trim s

                    proItem =
                        case nameAndValue of
                            n :: val :: [] ->
                                VCons 0 (n |> trim |> toList |> stringToVCons)
                                        (trim val 
                                        |> split " "
                                        |> map trim
                                        |> map toList 
                                        |> map stringToVCons
                                        |> foldr (VCons 0) (VNil 0))

                            _ ->
                                VError "Parse Style Error."
                in
                case proItem of
                    VError _ ->
                        parseStyle ps
                    
                    _ ->
                        VCons 0 proItem (parseStyle ps)

        [] -> VNil 0


stringToVCons :  List Char -> Value
stringToVCons lc =
    case lc of
        [] ->
            VNil 1

        c :: cs ->  VCons 1 (VChar c) (stringToVCons cs)


parseOtherPro : List Attribute -> Value
parseOtherPro al =
    case al of
        ("contenteditable", _) :: al_ ->
            parseOtherPro al_ 

        (name, value) :: al_ ->
            if (trim name |> left 1) == "{" then
                let
                    proItem =
                        name |> trim |> toList |> stringToVCons 
                in
                    VCons 0 proItem (parseOtherPro al_)
            else
                let
                    proItem =
                        VCons 0 (name |> trim |> toList |> stringToVCons)
                        (VCons 0 (value |> trim |> toList |> stringToVCons)
                            (VNil 0))
                in
                    VCons 0 proItem (parseOtherPro al_)
        
        [] ->
            VNil 0