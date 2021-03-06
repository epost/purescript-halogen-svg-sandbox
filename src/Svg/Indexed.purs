module Svg.Indexed where

import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

-- Attributes based on Mozilla MDN categories

type CoreAttributes r = (id :: String | r)

type StyleAttributes r = ("class" :: String | r)

-- Subset of events that work on Firefox 60/Chromium 66
type GlobalEventAttributes r =
  ( onClick :: MouseEvent
  , onDoubleClick :: MouseEvent
  , onContextMenu :: MouseEvent
  , onKeyDown :: KeyboardEvent
  , onKeyPress :: KeyboardEvent
  , onKeyUp :: KeyboardEvent
  , onMouseDown :: MouseEvent
  , onMouseEnter :: MouseEvent
  , onMouseLeave :: MouseEvent
  , onMouseMove :: MouseEvent
  , onMouseOut :: MouseEvent
  , onMouseOver :: MouseEvent
  , onMouseUp :: MouseEvent
  , onWheel :: WheelEvent
  | r)

-- These can also be done with CSS
type PresentationAttributes r = (stroke :: String, fill :: String | r)

type GlobalAttributes r = (PresentationAttributes (GlobalEventAttributes (StyleAttributes (CoreAttributes r))))

type SVGsvg = GlobalAttributes
  ( width :: Number
  , height :: Number
  , viewBox :: String
  , preserveAspectRatio :: String
  )

type SVGcircle = GlobalAttributes
  ( cx :: Number
  , cy :: Number
  , r :: Number
  , transform :: String
  )

type SVGellipse = GlobalAttributes
  ( cx :: Number
  , cy :: Number
  , rx :: Number
  , ry :: Number
  , transform :: String
  )

type SVGrect = GlobalAttributes
  ( x :: Number
  , y :: Number
  , rx :: Number
  , ry :: Number
  , width :: Number
  , height :: Number
  , transform :: String
  )

type SVGg = GlobalAttributes
  ( transform :: String )

type SVGpath = GlobalAttributes
  ( d :: String
  , transform :: String
  )

type SVGline = GlobalAttributes
  ( x1 :: Number
  , y1 :: Number
  , x2 :: Number
  , y2 :: Number
  , transform :: String
  )

type SVGtext = GlobalAttributes
  ( x :: Number
  , y :: Number
  , text_anchor :: String
  , dominant_baseline :: String
  , transform :: String
  , font_size :: String
  )

type SVGforeignObject = GlobalAttributes
  ( x :: Number
  , y :: Number
  , height :: Number
  , width :: Number
  )

--------------------------------------------------------------------------------

type SVGanimate = GlobalAttributes
  ( attributeName :: String
  , from :: String
  , to :: String
  , begin :: String
  , dur :: String
  , repeatCount :: Int
  , fill :: String
  )

-- TODO dedupe wrt SVGanimate above
type SVGanimateMotion = GlobalAttributes
  ( id :: String
  -- , attributeName :: String
  -- , from :: String
  -- , to :: String
  , begin :: String
  , dur :: String
  , repeatCount :: Int
  , fill :: String
  )

type SVGmpath = GlobalAttributes
  ( xlinkHref :: String
  )
