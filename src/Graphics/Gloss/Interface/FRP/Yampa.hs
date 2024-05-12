{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright  : (c) 2018-2023 Ivan Perez
--              (c) 2015-2018 Konstantin Saveljev
-- License    : MIT License (MIT)
-- Maintainer : ivan.perez@keera.co.uk
--
-- Gloss backend for Yampa.
--
-- Gloss is a purely functional library to create pictures and animations.
-- Yampa is a Functional Reactive Programming DSL structured around signal
-- functions.
--
-- This module provides a function to create an interactive Gloss animation
-- driven by a signal function that transforms a Gloss input signal into a
-- Gloss 'Picture'.
module Graphics.Gloss.Interface.FRP.Yampa
    (InputEvent, playYampa, reactInitInteract)
  where

-- External imports
import           Control.Monad                    (when,void,forever)
import           Data.IORef                       (newIORef, readIORef,
                                                   writeIORef, modifyIORef)
import           FRP.Yampa                        (DTime, Event (..), SF, react,
                                                   reactInit, ReactHandle, (&&&), returnA, time)
import           Graphics.Gloss                   (Color, Display, Picture,
                                                   blank)
import           Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Graphics.Gloss.Interface.IO.Interact as Gloss.Interact
import Graphics.Gloss.Interface.IO.Interact (controllerSetRedraw)
import Control.Concurrent
import Control.Monad
import System.CPUTime
import System.IO
import System.Timeout
--import Control.Concurrent.QSem

-- | Type representing input events to the signal function.
--
-- Note that this type represents the kind of information placed inside the
-- Yampa 'Event'. It will still be wrapped in an 'Event' to represent the fact
-- that an 'InputEvent' may or may not be present at one particular point in
-- time, and that it changes discretely.
type InputEvent = G.Event

-- | Play the game in a window, updating when the value of the provided
playYampa :: Display                       -- ^ The display method
          -> Color                         -- ^ The background color
          -> Int                           -- ^ The refresh rate, in Hertz
          -> SF (Event InputEvent) Picture -- ^ Signal function
          -> IO ()
playYampa display color frequency mainSF = do
  picRef <- newIORef blank

  handle <- reactInit
              (return NoEvent)
              (\_ updated pic -> do when updated (picRef `writeIORef` pic)
                                    return False
              )
              mainSF

  let -- An action to convert the world to a picture
      toPic :: DTime -> IO Picture
      toPic = const $ readIORef picRef

      -- A function to handle input events
      handleInput :: G.Event -> DTime -> IO DTime
      handleInput event timeAcc = do
          _quit <- react handle (delta, Just (Event event))
          return (timeAcc + delta)
        where
          delta = 0.01 / fromIntegral frequency

      -- A function to step the world one iteration. It is passed the period of
      -- time (in seconds) needing to be advanced
      stepWorld :: Float -> DTime -> IO DTime
      stepWorld delta timeAcc
          | delta' > 0 = react handle (delta', Just NoEvent) >> return 0.0
          | otherwise  = return (-delta')
        where
          delta' = realToFrac delta - timeAcc

  playIO display color frequency 0 toPic handleInput stepWorld


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
reactInitInteract ::

    -- | Viewport style.
    Display ->

    -- | Background color.
    Color ->

    -- | Initial state.
    s ->

    -- | Description of how the picture changes.
    SF s Picture ->

    -- | A handle to react to inputs from Gloss.
    (ReactHandle (Event InputEvent) a) ->

    -- | We return a handle to generate pictures.
    IO (ReactHandle s Picture)

{-# INLINABLE reactInitInteract #-}

reactInitInteract display color state sf callbackHandle = do
    picRef   <- newIORef mempty
    redrawFn <- newIORef (pure ())
    _        <- forkIO do
                        Gloss.Interact.interactIO
                            display
                            color
                            picRef
                            readIORef
                            handleEvent
                            (writeIORef redrawFn . controllerSetRedraw)
    reactInit (pure state) (actuate redrawFn picRef) sf
  where
    actuate redrawFnRef picRef handle updated pic = do
        when updated do
            -- we pass the picture generated in yampa to the draw fn
            writeIORef picRef pic
            -- we run the redraw function
            join (readIORef redrawFnRef)
        pure False
    handleEvent ev s = do
        react callbackHandle (0.0, Just (Event ev))
        pure s


-- TODO move somewhere else?
_testInteractYampa :: IO ()
_testInteractYampa = do
    let fps    = 60
    let dt     = 1 / realToFrac fps
    let tdelay = round (realToFrac 1000000 * dt)
    eventCallbackHandle <- reactInit (pure NoEvent) (\_ updated ev -> when updated (print ev) >> pure False) returnA
    h <- reactInitInteract
            Gloss.Interact.FullScreen
            Gloss.Interact.white
            0
            (proc cpuTime -> do
                    localTime <- time -< ()
                    returnA -< Gloss.Interact.Pictures
                                     [ Gloss.Interact.circle (10 * realToFrac localTime)
                                     , Gloss.Interact.scale 0.7 0.7 (Gloss.Interact.text (show cpuTime))
                                     ]
            )
            eventCallbackHandle
    forever do
        threadDelay tdelay
        t <- getCPUTime
        react h (dt, Just t) 

