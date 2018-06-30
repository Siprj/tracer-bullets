module Lib
    ( someFunc
    ) where

import Data.Maybe (fromJust)
import Data.StateVar
import Control.Monad (unless, when, void)
import System.Exit (exitFailure)
-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
-- gl, all types and funcs here will already start with "gl"

someFunc :: IO ()
someFunc = do
    GLFW.init
    primaryMonitor <- fromJust <$> GLFW.getPrimaryMonitor
--    print primaryMonitor
    supportedVideoModes <- GLFW.getVideoModes primaryMonitor
    let (GLFW.VideoMode width height red green blue refreshRate) =
            maximum $ maximum supportedVideoModes
    GLFW.windowHint $ GLFW.WindowHint'RedBits red
    GLFW.windowHint $ GLFW.WindowHint'GreenBits green
    GLFW.windowHint $ GLFW.WindowHint'BlueBits blue
    GLFW.windowHint $ GLFW.WindowHint'RefreshRate refreshRate
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    window <- GLFW.createWindow width height
        "blabla" (Just primaryMonitor) Nothing
    maybe printErrorAndFail setAndloop window
    GLFW.terminate
  where
    printErrorAndFail = print "Can't create window" >> exitFailure

    setAndloop window = do
        putStrLn "alsdfjlaksjdflkajsdf "
        GLFW.setKeyCallback window (Just callback)
        GLFW.makeContextCurrent (Just window)
        loop window

    loop window = do
        shouldClose <- GLFW.windowShouldClose window
        unless shouldClose $ do
            GLFW.pollEvents
            GL.clearColor $= (GL.Color4 0.2 0.3 0.3 1.0)
            GL.clear [GL.ColorBuffer]
            GLFW.swapBuffers window
            loop window


callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
