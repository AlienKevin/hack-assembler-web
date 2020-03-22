port module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Element as E exposing (Element)
import Element.Input as Input
import Element.Font as Font

port sendInput : String -> Cmd msg
port receiveOutput : (Output -> msg) -> Sub msg

type alias Model =
  { source: String
  , output: String
  }

type Msg
  = EditSource String
  | NoEditOutput String
  | ReceiveOutput Output

type alias Output =
  { code: Maybe String, errorMessage: Maybe String }

main : Program () Model Msg
main  =
  Browser.element { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

init : () -> (Model, Cmd Msg)
init () =
  ({ source = ""
  , output = ""
  }
  , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EditSource newSource ->
      ({ model |
        source = newSource
      }
      , sendInput newSource
      )
    NoEditOutput _ ->
      (model, Cmd.none)
    ReceiveOutput output ->
      ({ model |
        output =
          case output.code of
            Just code ->
              code
            Nothing ->
              Maybe.withDefault "I found an error in the inputted Hack assembly code." output.errorMessage
      }
      , Cmd.none
      )
  
subscriptions: Model -> Sub Msg
subscriptions _ =
  receiveOutput ReceiveOutput

view : Model -> Html Msg
view model =
  E.layout
    [ Font.family
      [ Font.typeface "Inconsolata"
      , Font.monospace
      ]
    ] <|
    E.column
    [ E.width E.fill
    , E.height E.fill
    , E.padding 30
    ]
    [ E.el
      [ Font.size 25
      , Font.extraLight
      ]
      <| E.text "Hack Assembler"
    , E.row
    [ E.width E.fill
    , E.htmlAttribute <| Html.Attributes.style "height" "80vh"
    , E.centerY
    , E.spacing 30
    ]
    [ sourcePane model
    , outputPane model
    ]
    ]

sourcePane : Model -> Element Msg
sourcePane model =
  Input.multiline
    [ E.height E.fill
    , E.clipX
    ]
    { onChange = EditSource
    , text = model.source
    , placeholder = Just (Input.placeholder [] <| E.text "Write symbolic Hack assembly here...")
    , label = Input.labelAbove [] <| E.text "Symbolic"
    , spellcheck = False
    }

outputPane : Model -> Element Msg
outputPane model =
  Input.multiline
    [ E.height E.fill
    , E.clipX
    ]
    { onChange = NoEditOutput
    , text = model.output
    , placeholder = Just (Input.placeholder [] <| E.text "You can find outputed binary Hack assembly here...")
    , label = Input.labelAbove [] <| E.text "Binary"
    , spellcheck = False
    }