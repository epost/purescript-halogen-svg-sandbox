module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import SvgAnimateMotion1 as SvgAnimateMotion1

initialState :: SvgAnimateMotion1.State
initialState = { msg: "" }

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI (SvgAnimateMotion1.ui initialState) unit body
