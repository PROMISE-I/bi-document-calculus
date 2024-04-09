module HtmlParser exposing (..)

import Syntax exposing (Value(..))
import Html.Parser exposing (run, Node(..), Attribute)
import String exposing (toList, trim, left)

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
                                VNode s (VNil 0) vChilds

                            al -> 
                                VNode s 
                                (parsePro al) vChilds

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


stringToVCons :  List Char -> Value
stringToVCons lc =
    case lc of
        [] ->
            VNil 1

        c :: cs ->  VCons 1 (VChar c) (stringToVCons cs)


parsePro : List Attribute -> Value
parsePro al =
    case al of
        ("contenteditable", _) :: al_ ->
            parsePro al_ 

        (name, value) :: al_ ->
            if (trim name |> left 1) == "{" then
                let
                    proItem =
                        name |> trim |> toList |> stringToVCons 
                in
                    VCons 0 proItem (parsePro al_)
            else
                let
                    proItem =
                        VCons 0 (name |> trim |> toList |> stringToVCons)
                        (VCons 0 (value |> trim |> toList |> stringToVCons)
                            (VNil 0))
                in
                    VCons 0 proItem (parsePro al_)
        
        [] ->
            VNil 0