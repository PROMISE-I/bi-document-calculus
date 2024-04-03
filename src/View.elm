module View exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Utils exposing (nth)
import Debug exposing (toString)
import Model exposing (Msg(..), Model, Mode(..))
import LangUtils exposing (print)
import Syntax exposing (Value(..), IndexedVEnv, VEnv, caseN)


view : Model -> Html Msg
view model =
    Html.div [ Attr.id "bi-preview" ]
            [ Html.div [Attr.id "menu-bar"]

                        [ Html.div [ Attr.id "title" ]
                                    [ Html.text "Bidirectional Author" ]

                        , Html.button [ Attr.id "run-program"
                                    , Attr.class "btn" ]
                                    [Html.text "Eval"]
                        , Html.button [ Attr.class "btn uneval" 
                                    , Events.onClick Preview]
                                    [Html.text "Uneval and Preview"]
                        , Html.button [ Attr.class "btn uneval"
                                    , Events.onClick Update]
                                    [Html.text "Uneval and Update"]
                        , Html.button [ Attr.class "btn uneval" 
                                    , Events.onClick Revert]
                                    [Html.text "Revert Code"]
                        ]
            , Html.div [Attr.id "output"]
                        [ Html.div [Attr.class "area-title"]
                                [Html.text "OUTPUT"]
                        , Html.div [Attr.id "output-area"]
                                    [Html.textarea [  Attr.id "console-output"
                                                    , Events.onInput OutputChange ]
                                                    []]
                        ]
            ]


contextToOptions : List Value -> List (Html Msg)
contextToOptions context =
    case context of
        [] -> []
        _::ct -> contextToOptions ct


printEditItem : Int -> Value -> (Int, String) -> String
printEditItem n1 v1 (n2, s2) =
    if n2 == -1 || n1 /= n2 then
        print v1
    else
        s2
