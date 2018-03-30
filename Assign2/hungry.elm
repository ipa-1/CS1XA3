-- references
-- http://package.elm-lang.org/packages/elm-lang/core/5.1.1
-- http://package.elm-lang.org/packages/elm-lang/svg/latest/Svg
-- https://guide.elm-lang.org/
-- different functions http://elm-lang.org/examples

-- using lists http://package.elm-lang.org/packages/circuithub/elm-list-extra/3.7.0/List-Extra#elemIndex
-- elm make https://github.com/elm-lang/elm-make
-- user input https://guide.elm-lang.org/architecture/user_input/

---- http://elm-lang.org/examples

module Main exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html as Html
-- import Html exposing (..)

import Char exposing (..)
import Mouse exposing (..)
import List exposing (..)
import Random exposing(..)
import Platform.Cmd as Cmd 
import Platform.Sub as Sub

import Svg exposing (..)
import Svg.Attributes exposing (..)

import AnimationFrame as Anim
import Keyboard as Key

-- styling -----------------------------------------------------------------------------------
words = Html.Attributes.style [("font-size","50px"),("text-align","center")]
nwords = Html.Attributes.style [("font-size","15px"),("text-align","center")]
divStyle = Html.Attributes.style [("width","100%"),("height","100%")]
toRight = Html.Attributes.style[("float","right"),("width","50%"),("height","100%")]
toLeft = Html.Attributes.style[("float","left"),("width","50%"),("height","100%")]
guideline = Html.Attributes.style[("width","100%"),("height","600px"),("border-style","solid solid solid solid"),("border-color","black"),("margin","10px")]

-- types -------------------------------------------------------------------------------------------------
-- defining types for variables needed in app

type alias Model = {
                    counter : Float
                    ,state : Int
                    ,position:{x:Int, y:Int}
                    ,points:Int
                    ,food:{x1:Int,y1:Int}
                    }
                     -- This is a type called "Model" that holds variables that we need to 
                     -- keep track of, sort of like the state of the page

type Msg = Tick Float | Starteat | KeyMsg Key.KeyCode | RandResult Int

-- views ------------------------------------------------------------------------------------------------
-- this is what the html will look like

view : Model -> Html.Html Msg 
view model = case model.state of
  0 -> start model
  1 -> room1 model
  2 -> end model model.points

  _ -> start model

room1 : Model -> Html.Html Msg
room1 model =let
  xv = toString model.position.x
  yv = toString model.position.y
  px1 = toString model.food.x1
  py1 = toString model.food.y1
 in 
   Html.div [] 
    [
         Html.div [] [
               Html.div [toRight,words] [
                       Html.p [] [Html.text ( toString (round <| model.counter/100))]
                       --,Html.p [] [Html.text(toString model.position)]
                       --,Html.p [] [Html.text(toString model.food)]
                       ,Html.p [] [Html.text "Food count: ",Html.text(toString model.points)]
                                       ]
               ,Html.div [toLeft] [
               svg [guideline][
                     ellipse[cx px1, cy py1, rx "5", ry"5", fill "red"] []
                     ,ellipse [cx xv, cy yv , rx "20", ry "20", fill "blue" ] []
                    --,rect [x "20", y "20", Html.Attributes.width 500, Html.Attributes.height 500] []
                     ]
                  --,Html.text (toString model.position) 
                ]
         ]
    ]

start : Model -> Html.Html Msg
start model = div [words,divStyle]
 [ p [] [Html.text " Oh no! Your last class ends at 2:20 ... You are so hungry that you turn into a blue circle. "]
  ,p [nwords] [Html.text " Use your arrow keys to eat the delicious small red apple"]
 , button [onClick Starteat] [Html.text "Ok."]
 ]

end : Model -> Int -> Html.Html Msg
end model points = let
   hungerlvl = case points of
    0 -> "Dying"
    1 -> "Starving"
    2 -> "Famished"
    3 -> "Very Hungry"
    4 -> "Hungry"
    5 -> "Not Hungry"
    6 -> "Fine"
    7 -> "Satiated"
    8 -> "Full Up"
    9 -> "Very Full"
    10 -> "Bloated"
    _-> "Very Bloated"

  in
   div [words,divStyle]
    [ p [] [Html.text " Time's up "]
     ,p [nwords] [Html.text " Score: ",Html.text(toString model.points)]
     ,p [nwords] [Html.p [] [Html.text "You are: ",Html.text hungerlvl]]
     ,button [onClick Starteat] [Html.text "Try Again!"]
    ]


-- initial model ----------------------------------------------------------------------------------------
-- init : (Model, Cmd Msg) -- initial model
-- this is what the model looks like right when the app starts

init = ({state = 0,counter = 1000,position = {x=300,y=300},points=0,food={x1=200,y1=200}}, Cmd.none)

-- update function ---------------------------------------------------------------------------------------
-- this is how the model will change based on inputs/other things

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of

  (Tick time) ->
   if (model.state == 1 && model.counter > 0 )
    then

     if ((abs(model.food.x1-model.position.x) <= 20 && abs(model.food.y1-model.position.y) <= 20))
      then ({model|counter=model.counter-1,points = model.points+1},generate RandResult(int 20 590))

     else ({model|counter=model.counter-1},Cmd.none)

   else if (model.state == 0) then (model,Cmd.none)

   else ({model|state=2,counter=0},Cmd.none)


  Starteat -> ({state=1,counter=1000,position={x=300,y=300},points=0,food={x1=200,y1=200}}, Cmd.none)

  KeyMsg p -> 
    if (model.position.x-10 < 20)
     then ({model|position={x = model.position.x+10, y = model.position.y}},Cmd.none)
    else if (model.position.x+10 > 670)
     then ({model|position={x = model.position.x-10, y = model.position.y}},Cmd.none)
    else if (model.position.y > 580 )
     then ({model|position={x = model.position.x, y = model.position.y-10}},Cmd.none)
    else if (model.position.y < 20)
     then ({model|position={x = model.position.x, y = model.position.y+10}},Cmd.none)
    else
     case p of
      37 -> ({model|position={x = model.position.x-10, y = model.position.y}}, Cmd.none)
      38 -> ({model|position={x = model.position.x, y = model.position.y-10}}, Cmd.none)
      39 -> ({model|position={x = model.position.x+10, y = model.position.y}}, Cmd.none)
      40 -> ({model|position={x = model.position.x, y = model.position.y+10}}, Cmd.none)
      _ -> (model, Cmd.none)

  RandResult res-> ({model|position={x=model.position.x,y=res},food={x1=res,y1=model.food.y1}},Cmd.none)

-- subscriptions -------------------------------------------------------------------------------------------
subscriptions model = Sub.batch[Anim.times Tick, Key.downs KeyMsg]

-- main -----------------------------------------------------------------------------------------------------
main = Html.program
 { init = init
 ,update = update
 ,view = view
 ,subscriptions = subscriptions}

