module Svg.Attributes where
-- Like Halogen.HTML.Properties

import Prelude
import Data.Maybe (Maybe(..))
import Data.String (joinWith, toUpper)

import Core as Core

import Halogen.HTML.Core (Prop, AttrName(AttrName))
import Halogen.HTML.Properties (IProp)
import Unsafe.Coerce (unsafeCoerce)

data Color = RGB Int Int Int

printColor :: Maybe Color -> String
printColor (Just (RGB r g b)) = "rgb(" <> (joinWith "," $ map show [r, g, b]) <> ")"
printColor Nothing = "None"

data Transform
  = Matrix Number Number Number Number Number Number
  | Translate Number Number
  | Scale Number Number
  | Rotate Number Number Number
  | SkewX Number
  | SkewY Number

data TextAnchor = Start | AnchorMiddle | End

data CSSLength
  = Cm Number
  | Mm Number
  | Inches Number
  | Px Number
  | Pt Number
  | Pc Number
  | Em Number
  | Ex Number
  | Rem Number
  | Vw Number
  | Vh Number
  | Vmin Number
  | Vmax Number
  | Pct Number
  | Nil

instance showCSSLength :: Show CSSLength where
  show (Cm i) = (show i) <> "cm"
  show (Mm i) = (show i) <> "mm"
  show (Inches i) = (show i) <> "in"
  show (Px i) = (show i) <> "px"
  show (Pt i) = (show i) <> "pt"
  show (Pc i) = (show i) <> "pc"
  show (Em i) = (show i) <> "em"
  show (Ex i) = (show i) <> "ex"
  show (Rem i) = (show i) <> "rem"
  show (Vw i) = (show i) <> "vw"
  show (Vh i) = (show i) <> "vh"
  show (Vmin i) = (show i) <> "vmin"
  show (Vmax i) = (show i) <> "vmax"
  show (Pct i) = (show i) <> "%"
  show Nil = "0"


data FontSize
  = XXSmall
  | XSmall
  | Small
  | Medium
  | Large
  | XLarge
  | XXLarge
  | Smaller
  | Larger
  | FontSizeLength CSSLength

instance showFontSize :: Show FontSize where
  show XXSmall = "xx-small"
  show XSmall = "x-small"
  show Small = "small"
  show Medium = "medium"
  show Large = "large"
  show XLarge = "x-large"
  show XXLarge = "xx-large"
  show Smaller = "smaller"
  show Larger = "larger"
  show (FontSizeLength l) = show l

printTextAnchor :: TextAnchor -> String
printTextAnchor Start = "start"
printTextAnchor AnchorMiddle = "middle"
printTextAnchor End = "end"

data Baseline
  = Auto | UseScript | NoChange | ResetSize | Ideographic | Alphabetic | Hanging
  | Mathematical | Central | BaselineMiddle | TextAfterEdge | TextBeforeEdge
printBaseline :: Baseline -> String
printBaseline Auto = "auto"
printBaseline UseScript = "use-script"
printBaseline NoChange = "no-change"
printBaseline ResetSize = "reset-size"
printBaseline Ideographic = "ideographic"
printBaseline Alphabetic = "alphabetic"
printBaseline Hanging = "hanging"
printBaseline Mathematical = "mathematical"
printBaseline Central = "central"
printBaseline BaselineMiddle = "middle"
printBaseline TextAfterEdge = "text-after-edge"
printBaseline TextBeforeEdge = "text-before-edge"

printTransform :: Transform -> String
printTransform (Matrix a b c d e f) =
  "matrix(" <> (joinWith "," $ map show [a, b, c, d, e, f]) <> ")"
printTransform (Translate x y) = "translate(" <> (joinWith "," $ map show [x, y]) <> ")"
printTransform (Scale x y) = "scale(" <> (joinWith "," $ map show [x, y]) <> ")"
printTransform (Rotate a x y) = "rotate(" <> (joinWith "," $ map show [a, x, y]) <> ")"
printTransform (SkewX a) = "skewX(" <> show a <> ")"
printTransform (SkewY a) = "skewY(" <> show a <> ")"

data D = Rel Command | Abs Command
printD :: D -> String
printD (Abs cmd) = (toUpper p.command) <> p.params
  where p = printCommand cmd
printD (Rel cmd) = p.command <> p.params
  where p = printCommand cmd

data Command
  = M Number Number
  | L Number Number
  | C Number Number Number Number Number Number
  | S Number Number Number Number
  | Q Number Number Number Number
  | T Number Number
  | A Number Number Number Boolean Boolean Number Number
  | Z

printCommand :: Command -> {command :: String, params :: String}
printCommand (M x y) = {command: "m", params: joinWith "," $ map show [x, y]}
printCommand (L x y) = {command: "l", params: joinWith "," $ map show [x, y]}
printCommand (C x1 y1 x2 y2 x y) =
  {command: "c" , params: joinWith "," $ map show [x1, y1, x2, y2, x, y]}
printCommand (S x2 y2 x y) =
  {command: "s" , params: joinWith "," $ map show [x2, y2, x, y]}
printCommand (Q x1 y1 x y) =
  {command: "q" , params: joinWith "," $ map show [x1, y1, x, y]}
printCommand (T x y) = {command: "t", params: joinWith "," $ map show [x, y]}
printCommand (A rx ry rot large sweep x y) =
  {command: "a", params: joinWith ","
                 $ map show [ rx, ry, rot ]
                 <> [ large_flag, sweep_flag ]
                 <> map show [ x, y ]}
  where
  large_flag = if large then "0" else "1"
  sweep_flag = if sweep then "0" else "1"
printCommand Z = {command: "z", params: ""}

data Align = Min | Mid | Max

printAlign :: Align -> String
printAlign Min = "Min"
printAlign Mid = "Mid"
printAlign Max = "Max"

data MeetOrSlice = Meet | Slice
printMeetOrSlice :: MeetOrSlice -> String
printMeetOrSlice Meet = "meet"
printMeetOrSlice Slice = "slice"

attr :: forall r i. AttrName -> String -> IProp r i
attr = coe Core.attr
  where
    coe :: (AttrName -> String -> Prop i) -> AttrName -> String -> IProp r i
    coe = unsafeCoerce

cx :: forall r i. Number -> IProp (cx :: Number | r) i
cx = attr (AttrName "cx") <<< show

cy :: forall r i. Number -> IProp (cy :: Number | r) i
cy = attr (AttrName "cy") <<< show

r :: forall s i. Number -> IProp (r :: Number | s) i
r = attr (AttrName "r") <<< show

viewBox :: forall r i. Number -> Number -> Number -> Number -> IProp (viewBox :: String | r) i
viewBox x y w h = attr (AttrName "viewBox") (joinWith " " $ map show [x, y, w, h])

preserveAspectRatio :: forall r i. Maybe {x :: Align, y :: Align} -> MeetOrSlice -> IProp (preserveAspectRatio :: String | r) i
preserveAspectRatio align slice =
  attr (AttrName "preserveAspectRatio") (joinWith " " $ [align_str, printMeetOrSlice slice])
  where
    align_str = case align of
      Nothing -> "none"
      Just {x, y} -> joinWith "" $ ["x", printAlign x, "Y", printAlign y]

rx :: forall r i. Number -> IProp (rx :: Number | r) i
rx = attr (AttrName "rx") <<< show

ry :: forall r i. Number -> IProp (ry :: Number | r) i
ry = attr (AttrName "ry") <<< show

width :: forall r i. Number -> IProp (width :: Number | r) i
width = attr (AttrName "width") <<< show

height :: forall r i. Number -> IProp (height :: Number | r) i
height = attr (AttrName "height") <<< show

x :: forall r i. Number -> IProp (x :: Number | r) i
x = attr (AttrName "x") <<< show

y :: forall r i. Number -> IProp (y :: Number | r) i
y = attr (AttrName "y") <<< show

x1 :: forall r i. Number -> IProp (x1 :: Number | r) i
x1 = attr (AttrName "x1") <<< show

y1 :: forall r i. Number -> IProp (y1 :: Number | r) i
y1 = attr (AttrName "y1") <<< show

x2 :: forall r i. Number -> IProp (x2 :: Number | r) i
x2 = attr (AttrName "x2") <<< show

y2 :: forall r i. Number -> IProp (y2 :: Number | r) i
y2 = attr (AttrName "y2") <<< show

stroke :: forall r i. Maybe Color -> IProp (stroke :: String | r) i
stroke = attr (AttrName "stroke") <<< printColor

fill :: forall r i. Maybe Color -> IProp (fill :: String | r) i
fill = attr (AttrName "fill") <<< printColor

transform :: forall r i . Array Transform -> IProp (transform :: String | r) i
transform = attr (AttrName "transform") <<< joinWith " " <<< map printTransform

d :: forall r i . Array D -> IProp (d :: String | r) i
d = attr (AttrName "d") <<< joinWith " " <<< map printD

text_anchor :: forall r i . TextAnchor -> IProp (text_anchor :: String | r) i
text_anchor = attr (AttrName "text-anchor") <<< printTextAnchor

font_size :: forall r i. FontSize -> IProp (font_size :: String | r) i
font_size = attr (AttrName "font-size") <<< show

dominant_baseline :: forall r i . Baseline -> IProp (transform :: String | r) i
dominant_baseline = attr (AttrName "dominant-baseline") <<< printBaseline

class_ :: forall r i . String -> IProp (class :: String | r) i
class_ = attr (AttrName "class")

id :: forall r i . String -> IProp (id :: String | r) i
id = attr (AttrName "id")

--------------------------------------------------------------------------------

-- TODO this should only have the `Duration` constructor, and then smart constructors on top
-- | https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dur
data Duration
  = Hours   Number -- TODO rm
  | Minutes Number -- TODO rm
  | Seconds Number -- TODO rm
  | Millis  Number -- TODO rm
  | Duration (Maybe Number) (Maybe Number) (Maybe Number) (Maybe Number) -- ^ TODO

-- TODO this should only have the product thing, and then smart constructors on top
printDuration :: Duration -> String
printDuration = case _ of
  Hours   h        -> show h <> "h"
  Minutes m        -> show m <> "m"
  Seconds s        -> show s <> "s"
  Millis  i        -> show i <> "i"
  Duration h m s i -> "TODO"


dur :: forall r i. Duration -> IProp (dur :: String | r) i
dur = attr (AttrName "dur") <<< printDuration

-- TODO ADT or free string?
attributeName :: forall r i. String -> IProp (attributeName :: String | r) i
attributeName = attr (AttrName "attributeName")

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/from
from :: forall r i. String -> IProp (from :: String | r) i
from = attr (AttrName "from")

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/to
to :: forall r i. String -> IProp (to :: String | r) i
to = attr (AttrName "to")

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/begin
begin :: forall r i. String -> IProp (begin :: String | r) i
begin = attr (AttrName "begin")

repeatCount :: forall r i. Int -> IProp (repeatCount :: Int | r) i
repeatCount = attr (AttrName "repeatCount") <<< show

fill_freeze :: forall r i. String -> IProp (fill :: String | r) i
fill_freeze = attr (AttrName "fill")

xlinkHref :: forall r i. String -> IProp (xlinkHref :: String | r) i
xlinkHref = attr (AttrName "xlink:href")
