{-# LANGUAGE NoImplicitPrelude #-}

module Shader () where

import Control.Applicative (pure)
import Control.Monad ((>>), (>>=), unless, when)
import Data.Bool (Bool)
import Data.ByteString (ByteString, readFile)
import Data.Function (($))
import Data.Maybe (Maybe(Just))
import Data.StateVar (($=))
import Data.String (String)
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
    (linkStatus, linkProgram, createProgram)
import qualified Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects as GL (Shader)
import System.Exit (exitFailure)
import System.IO (IO, FilePath, putStrLn)

data Shader = Shader
    { setup :: IO ()
    , tearDown :: IO ()
    , program :: Program
    }

compileProgramFile :: FilePath -> FilePath -> IO Program
compileProgramFile vertexShaderFileName fragmenShaderFileName = do
    vertexShaderText <- readFile vertexShaderFileName
    fragmenShaderText <- readFile fragmenShaderFileName

    compileProgram vertexShaderText fragmenShaderText

compileProgram :: ByteString -> ByteString -> IO Program
compileProgram vertexShaderText fragmentShaderText = do
    vertexShader <- createShader VertexShader
    shaderSourceBS vertexShader $= vertexShaderText
    compileShader vertexShader
    failShaderOnError vertexShader "ERROR Vertec shader compilation FAILED."

    fragmentShader <- createShader FragmentShader
    shaderSourceBS fragmentShader $= fragmentShaderText
    compileShader fragmentShader
    failShaderOnError fragmentShader "Fragmen shader compilation FAILED."

    shaderProgram <- createProgram
    attachShader shaderProgram vertexShader
    attachShader shaderProgram fragmentShader
    linkProgram shaderProgram
    failProgramOnError shaderProgram "Shader program compilation FAILED."

    currentProgram $= Just shaderProgram

    deleteObjectName vertexShader
    deleteObjectName fragmentShader
    pure shaderProgram

failShaderOnError :: GL.Shader -> String -> IO ()
failShaderOnError shader str = do
    infoLog <- get $ shaderInfoLog shader
    status <- compileStatus shader
    unless status $ putStrLn infoLog >> exitFailure

failProgramOnError :: Program -> String -> IO ()
failProgramOnError program str = do
    infoLog <- get $ programInfoLog program
    status <- linkStatus program
    unless status $ putStrLn infoLog >> exitFailure
