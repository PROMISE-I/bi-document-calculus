module BDModel exposing(..)

import BDSyntax exposing (..)

type alias Code = String
type alias Output = String
type Mode = Console | HTML

type alias Model =
    { code : Code
    , output : Output
    , codeBackup : Code
    , isOutputChange : Bool
    , mode: Mode
    }


type Msg 
    = SaveCode Code
    | OutputChange Output
    | Preview
    | Update
    | Revert


initModel : Model
initModel = { code = ""
            , output = ""
            , codeBackup = ""
            , isOutputChange = False
            , mode = Console
            }