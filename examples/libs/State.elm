module State exposing (Action(..), apply)


type Action stateModel stateMsg transitionData
    = Update ( stateModel, Cmd stateMsg )
    | Transition transitionData
    | Exit


apply :
    { exit : ( model, Cmd msg ), modelMapper : stateModel -> model, msgMapper : stateMsg -> msg, transition : transitionData -> ( model, Cmd msg ) }
    -> Action stateModel stateMsg transitionData
    -> ( model, Cmd msg )
apply { exit, modelMapper, msgMapper, transition } action =
    case action of
        Exit ->
            exit

        Transition transitionData ->
            transition transitionData

        Update ( stateModel, stateMsg ) ->
            ( modelMapper stateModel, stateMsg |> Cmd.map msgMapper )
