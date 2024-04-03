port module Main exposing (..)

import View exposing(view)
import Browser exposing (..)
import Html exposing (Html)
import Model exposing (Msg(..), Model, Mode(..))
import Syntax exposing (Value(..))
import String exposing (left)
import Controller exposing (evalCodeToModel)
import Update exposing (updateCode)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ = (Model.initModel, Cmd.none)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveCode newCode ->
            let
                newModel =
                    evalCodeToModel newCode 

                cmd =
                    if newModel.mode == HTML then
                        sendOutput newModel.output
                    else
                        setConsoleVisible newModel.output
            in
            (newModel, cmd)

        OutputChange newOutput ->
            let
                (nop, flag, cmd) = 
                    if  model.mode == Console && left 9 newOutput == "<textarea" 
                    then
                        (model.output, False, Cmd.none)
                    else
                        (newOutput, True, setAceRed ())
            in
            ({model | output = nop
                    , isOutputChange = flag}, cmd)

        Preview ->
            let
                newCode = updateCode model
            in
            ({model | code = newCode
            }, sendCode (newCode, False))

        Update ->
            let
                newCode = updateCode model
            in
            ({model | code         = newCode
                    , codeBackup   = newCode
                    , isOutputChange = False
            }, sendCode (newCode, False))


        Revert ->
            ({model | code = model.codeBackup
            }, sendCode (model.codeBackup, model.isOutputChange))


view : Model -> Html Msg
view = View.view


port sendCode : (String, Bool) -> Cmd msg
port sendOutput : String -> Cmd msg
port setAceRed : () -> Cmd msg
port setConsoleVisible : String -> Cmd msg
port receiveCode : (String -> msg) -> Sub msg
port receiveOutput : (String -> msg) -> Sub msg

port setConsoleLog : (String) -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveCode SaveCode
        , receiveOutput OutputChange
        ]