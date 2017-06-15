import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram { model = init, view = view, update = update }

-- MODEL

type alias Model = { status : String }

init : Model
init = { status = "" }

-- UPDATE

type Msg = Btn

update : Msg -> Model -> Model
update msg model =
  case msg of
    Btn -> { model | status = "You win!" }

-- VIEW

view : Model -> Html Msg
view model = div []
  [ button [onClick Btn] [text "Press Me"]
  , text model.status
  ]
