{-# LANGUAGE NoImplicitPrelude #-}

module Shader.OpaqueShader where

import Control.Applicative (pure)
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.StateVar (($=), get)
import Graphics.GL.Types (GLfloat)
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects (Program, currentProgram)
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform (uniform, uniformLocation)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3)
import Numeric.Matrix
import System.IO (IO, FilePath, putStrLn)

import Shader.Compile (compileProgramFile)
import Graphics.Rendering.OpenGL.GL.DataFrame ()


data OpaqueShader = OpaqueShader
    { setup :: IO ()
    , tearDown :: IO ()
    , setModelMatrix :: Mat44f -> IO ()
    , setViewMatrix :: Mat44f -> IO ()
    , setLightPosition :: Vector3 GLfloat -> IO ()
    , setProjectionMatrix :: Mat44f -> IO ()
    , setViewPosition :: Vector3 GLfloat -> IO ()
    }

opaqueVertexShader = "shader/opaque.vert"
opaqueFragmentShader = "shader/opaque.frag"

compileOpaqueShader :: IO OpaqueShader
compileOpaqueShader = do
    prog <- compileProgramFile opaqueVertexShader opaqueFragmentShader
    modelUniformLoc <- get $ uniformLocation prog "model"
    viewUniformLoc <- get $ uniformLocation prog "view"
    projectionUniformLoc <- get $ uniformLocation prog "projection"
    lightPosUniformLoc <- get $ uniformLocation prog "lightPos"
    viewPosUniformLoc <- get $ uniformLocation prog "viewPos"
    pure OpaqueShader
        { setup = currentProgram $= Just prog
        , tearDown = currentProgram $= Nothing
        , setModelMatrix = ($=) (uniform modelUniformLoc)
        , setViewMatrix = ($=) (uniform viewUniformLoc)
        , setLightPosition = ($=) (uniform lightPosUniformLoc)
        , setProjectionMatrix = ($=) (uniform projectionUniformLoc)
        , setViewPosition = ($=) (uniform viewPosUniformLoc)
        }
