{-# LANGUAGE FlexibleContexts    #-}

{-# LANGUAGE ScopedTypeVariables #-}
module App where

import           Data.Typeable
import           Monomer
import           OpenGLWidget
import           Types

handleEventComp  _wenv node compModel msg = case cast msg of
    Just (OpenGLWidgetInit gls) -> []

    Just (AppEvent (MoveY tY))  ->  []

    _                           -> []
buildUI
  ::
  WidgetEnv OpenGLComp OpenGLCompEvent
  -> OpenGLComp
  -> WidgetNode OpenGLComp OpenGLCompEvent

buildUI _wenv = openGLWidget
