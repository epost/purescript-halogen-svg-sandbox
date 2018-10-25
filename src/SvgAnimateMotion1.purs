module SvgAnimateMotion1 where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML (div)
import Halogen.HTML (HTML)
import Halogen.HTML.Events as HE
import Svg.Elements as SE
import Svg.Attributes as SA
import Svg.Attributes (Duration(..), printDuration)

type Vec2D = { x :: Number, y :: Number }

ui :: ∀ a. State -> H.Component HTML Query Unit Void a
ui initialState =
  H.component { initialState: const initialState, render, eval, receiver: const Nothing }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      div [ HP.classes [ ClassName "copy-me" ] -- class used by external js code to copy SVG content
          ]
          [ SE.svg [ leViewBox ]
                   [ trajectoryWithAnimatedCircle "tr_100" {x:10.0, y:30.0} {x:70.0, y:30.0} ]
          ]

-- | Create and draw a path along which to animate the circle.
trajectoryWithAnimatedCircle :: ∀ a. HtmlId -> Vec2D -> Vec2D -> HTML a (Query Unit)
trajectoryWithAnimatedCircle trajectoryId startPos endPos =
  SE.g []
       [ svgPath
       , animatedCircle
       ]
  where
    svgPath =
      SE.path [ SA.id trajectoryId
              , SA.d $ SA.Abs <$> [ SA.M startPos.x startPos.y, SA.L endPos.x endPos.y ]
              , SA.stroke $ Just (SA.RGB 200 200 200)
              ]

    -- An animated circle that moves from left to right and grows in size as it moves.
    animatedCircle :: HTML a (Query Unit)
    animatedCircle =
      SE.circleWithKids
        [ SA.cx     0.0 -- circle initially out of sight
        , SA.cy     0.0 -- circle initially out of sight
        , SA.r      3.0
        , SA.class_ "le_circle"
        , SA.fill   $ Just (SA.RGB 255 255 0)
        , SA.stroke $ Just (SA.RGB 150 150 150)
        ]
        [ SE.animateMotion -- move the circle from left to right
            [ SA.id animationId
            , SA.dur (Seconds 1.0)
            , SA.begin "indefinite" -- animation is triggered by external js call to .beginElement()
            , SA.repeatCount 1
            ]
            [ SE.mpath [ SA.xlinkHref $ "#" <> trajectoryId ] ] -- define trajectory by referencing an existing motion path
        , SE.animate -- grow the circle as it moves
            [ SA.dur (Seconds 0.25)
            , SA.attributeName "r"
            , SA.from "0.0"
            , SA.to "3.0"
            , SA.begin (animationId <> ".begin")
            , SA.repeatCount 0
            , SA.fill_freeze "freeze"
            ]
        ]
      where
        animationId = trajectoryId <> "_animation"

leViewBox =
  SA.viewBox sceneLeft sceneTop sceneWidth sceneHeight
  where
    sceneWidth  = (bounds.max.x - bounds.min.x) + paddingX
    sceneHeight = (bounds.max.y - bounds.min.y) + paddingY
    sceneLeft   = bounds.min.x - (paddingX / 2.0)
    sceneTop    = bounds.min.y - (paddingY / 2.0)
    bounds      = {min: {x:10.0, y:30.0}, max: {x:90.0, y:30.0}}
    paddingX    = 32.0
    paddingY    = 32.0

type HtmlId = String

--------------------------------------------------------------------------------

data Query a = ClickTransition a

type State = { msg :: String }

eval :: ∀ a. Query ~> H.ComponentDSL State Query Void a
eval (ClickTransition next) = pure next
