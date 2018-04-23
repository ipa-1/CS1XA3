# Elm Architecture

Name | Type Signature | Description
--- | --- | ---
view | view : Model -> Html.Html Msg |is a function that takes your model, and depending on its content, (the states) generates HTML. here you can use html elements such as Html.div or Html.text or svg
update | update : Msg -> model -> (Model, Cmd Msg) |is a function that takes your current model and updates/generates a new one depending on the msgs received, and returns its
type alias model = | | is a datatype that represents the state of your program/ keeps track of states
main | | is a function that generates a program
init | init : (Model.Cmd Msg) | is a model that represents the initial state, right when the program starts
type msg | | is usually defined to hold different messages that your update function may received

main : Program Never Model Msg
main = Html.beginnerProgram
     {model = init,
     view = view,
     update = update}
     






