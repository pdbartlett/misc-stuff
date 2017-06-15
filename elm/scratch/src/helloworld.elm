import Html exposing (Html, text)

main =
  Html.beginnerProgram { model = Nothing, view = view, update = update }

-- MODEL

type Model = Nothing

-- UPDATE

type Msg = Nop

update : Msg -> Model -> Model
update msg model =
  case msg of
    Nop -> model

-- VIEW

view : Model -> Html Msg
view model =
  text "Hello world!"
