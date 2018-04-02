module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

                   
navStyle = style [ -- style of nav
                 ("list-style-type","none")
                 ,("margin","0px")
                 ,("padding","10px") -- changes the wrapper around the items in the nav
                 ,("overflow","hidden") -- prevents list elements from going outside the list
                 ,("background-color","#333")
                 ]
                 
inlinenav = style [ -- style for the elements in nav
                  ("display","inline") -- <li> elements are block, this allows them to be displayed on one line
                  ,("font-size","20px")
                  ,("color","White")
                  ,("padding","8px 8px")
                  ,("margin","8px")
                  ,("background-color","#333")
                  ]


divstyle = style[

                ("padding","0px")
                ,("margin","0px")
                ,("width","100%")
                ,("position","fixed")
                --,("background-color","#333")
                ]

quoteStyle = style[
                  
                  ("text-align","center")
                  ,("height","800px")
                  ,("width", "100%")
                  --,("background-image","img [src "wat.jpg"][]")
                  --,("padding","250px")
                  ]

ssStyle = style [

                ("background-color","#f1f2e6")
                ,("width", "100%")
                ,("height","300px")
                ,("font","MS Trebuchet")
                --,("padding", "50px")
                ]

linkStyle = style[
                 ("color","White")
                 ,("text-decoration","none")
                 ]

sectionStyle = style[

                    ("padding-left","350px")
                    ,("padding-top","50px")
                    --,("width","600px")
                    ]

tableStyle = style[

                  ("padding", "20px 20px 0px 20px") -- top right bottom, left
                  ,("margin","0px")
                  --,("width","100px")
                  ]

titleStyle = style [

                   ("text-align","center" )

                   ]
main : Html msg
main = div [style[("height","100%"),("font-family", "MS Trebuchet")]] [ -- the big div holding everything

   div [divstyle] [
   ul [navStyle] [
      li [inlinenav] [text "Home"]
      ,li [inlinenav] [a [href "http://ugweb.cas.mcmaster.ca/~ipa1/eat.html",linkStyle] [text "App"]]
      ,li [inlinenav] [a [href "http://ugweb.cas.mcmaster.ca/~ipa1/contact.html",linkStyle] [text "Contact"]]
      ]
   ]
   
   
  ,div [quoteStyle][ 
  pre [] [br [] [] , br [] [] , br [] [] , br [] [],  br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,
   h1 [] [text "If you ever get cold, stand in the corner of the room ! It's 90 degrees "] -- should be put in a div later
   ,h1 [style[("font-size","50px")]] [text "Alice Ip "] 
   ,img [src "alice1.jpg",style[("width","170px"),("padding","50px 50px")]][]
  ]]
----------------------------------------------------------------------------------------------------------------------
  ,div[ssStyle] [ -- Second section on page
  
      aside [style[("float","right")]] [img [src "alice2.jpg",style[("width","170px"),("padding-top","100px"),("padding-right","250px")]][]] -- floating part
       
       ,div [sectionStyle] [ -- left side of second section

       h1 [titleStyle] [text "About Me"]
       ,p [titleStyle] [text "Photographer  ✿  Videographer  ✿  Illustrator  ✿  Musician  "] 
       ,p [] [text "Currently an undergraduate student at McMaster University majoring in Computer Science"

       ]
       ] -- end of left side of section section

  ] -- end of second section div
 --------------------------------------------------------------------------------------------------------------------- 
    ----------------------------------------------------------------------------------------------------------------------
  {-,div[ssStyle] [ -- Second quote on page
  
     div [quoteStyle][ 
       pre [] [br [] [] , br [] [] , br [] [] , br [] [],  br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,br [] [] ,
       h1 [] [text "What happened to the plant in math class? It grew square roots  "] -- should be put in a div later
  ]]

  ] -- end of second quote div-}
 --------------------------------------------------------------------------------------------------------------------- 
    ,div[style[("background-color","White"),("height","100%")]] [ -- Second section on page
  
      aside [style[("float","right")]] [img [src "alice3.jpg",style[("width","170px"),("padding","150px 250px 0px 0px")]][]] -- floating part
       
       ,div [sectionStyle] [ -- left side of third section

        h1 [titleStyle] [text "Work Experience"]
       ,table [] [
          tr [] [td [tableStyle] [b [] [text"ACM Printing and Distribution"]], td [tableStyle] [b [] [text"Inventory and Shipment Administrator"]]]
         ,tr [] [td [] [text"May 2017 - August 2017"], td [tableStyle] [text" Updated and managed inventory or something"]]
         ,tr [] [td [tableStyle] []]
         ,tr [] [td [tableStyle] [b [] [text"Dr Chan's Office"]], td [tableStyle] [b [] [text"Receptionist"]]]
         ,tr [] [td [] [text" August 2015- August 2016"], td [tableStyle] [text"Collected and rectified patient records on online computer database."
         , text "Scheduled and managed appointments on computer scheduling program to optimize service to customers"]]
      ]
   
       ] -- end of left side of third section

  ] -- end of second section div
  --------------------------------------------------------------------------------------------------------------------
     --------------------------------------------------------------------------------------------------------------------- 
    ,div[style[("background-color","White"),("height","1000px")]] [ -- Second section on page
  
        aside [style[("float","right")]] [img [src "alice4.jpg",style[("width","170px"),("padding","150px 250px 0px 0px")]][]] -- floating part
       
       ,div [sectionStyle] [ -- left side of third section

                 h1 [titleStyle] [text "Skills"]
                ,h2 [titleStyle] [text "Programming Languages"]
                ,table [] [
                      tr [] [td [] [text"Python"], td [] [text"@ @"]]
                     ,tr [] [td [tableStyle] []]
                     ,tr [] [td [] [text"Haskell"], td [] [text"@"]]
                     ,tr [] [td [tableStyle] []]
                     ,tr [] [td [] [text"HTML"], td [] [text"@"]]
                     ,tr [] [td [tableStyle] []]
                     ,tr [] [td [] [text"CSS"], td [] [text"@"]]
                     ,tr [] [td [tableStyle] []]
                     ,tr [] [td [] [text"Elm"], td [] [text"@"]]
                 ]
       ] -- end of left side of third section

       ,aside [style[("float","right")]] [img [src "alice5.jpg",style[("width","170px"),("padding","150px 250px 0px 0px")]][]] -- floating part

       ,div [sectionStyle] [ -- left side of fourth section

                    h2 [titleStyle] [text "Tools"]
                   ,table [] [
                      tr [] [td [] [text"Sublime Text"], td [] [text"@ "]]
                     ,tr [] [td [tableStyle] []]
                     ,tr [] [td [] [text"MS Office"], td [] [text"@"]]
                     ,tr [] [td [tableStyle] []]
                     ,tr [] [td [] [text"Adobe Photoshop"], td [] [text"@ @ @"]]
                       ]

                    ,br [] []
                    ,br [] []
                    ,br [] []
                   
       ] -- end of left side of fourth section

       ,aside [style[("float","right")]] [img [src "alice6.jpg",style[("width","170px"),("padding","150px 250px 0px 0px")]][]] -- floating part
       ,div [sectionStyle] [
                      h2 [titleStyle] [text " Languages"]
                      ,table [] [
                      tr [] [td [] [text"English"], td [] [text"@ @ @  "]]
                     ,tr [] [td [tableStyle] []]
                     ,tr [] [td [] [text"Cantonese"], td [] [text"@"]]
                     ]
       ]

           
  ] -- end of second section div
  --------------------------------------------------------------------------------------------------------------------
  --,footer [style[("text-align","center")]][b [] [text "Alice Ip Copyright 2018 "]]

  ] -- end of big div