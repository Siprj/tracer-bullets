{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Monad (unless, when, void)
import Data.Maybe (fromJust)
import Data.StateVar
import Data.ByteString (ByteString)
import qualified Data.Vector.Storable as V
import Foreign.Storable (sizeOf)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import System.Exit (exitFailure)
import System.IO (IO)
-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.BufferObjects as GL
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

        triangle <- GL.genObjectName
        GL.bindBuffer GL.ArrayBuffer $= Just triangle
        V.unsafeWith vertices $ \ptr -> do
            let size = fromIntegral
                    (V.length vertices * sizeOf (V.head vertices))
            GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

        vertexShader <- GL.createShader GL.VertexShader
        GL.shaderSourceBS vertexShader $= vertexShaderText
        GL.compileShader vertexShader
        verRes <- GL.compileStatus vertexShader
        unless verRes $ putStrLn "vertec shader compilation FAILED"
            >> exitFailure

        fragmentShader <- GL.createShader GL.FragmentShader
        GL.shaderSourceBS fragmentShader $= fragmentShaderText
        GL.compileShader fragmentShader
        fragRes <- GL.compileStatus fragmentShader
        unless verRes $ putStrLn "fragmen shader compilation FAILED"
            >> exitFailure

        shaderProgram <- GL.createProgram
        GL.attachShader shaderProgram vertexShader
        GL.attachShader shaderProgram fragmentShader
        GL.linkProgram shaderProgram
        GL.linkStatus shaderProgram
        GL.currentProgram $= Just shaderProgram

        GL.deleteObjectName vertexShader
        GL.deleteObjectName fragmentShader

        vao <- GL.genObjectName
        GL.bindVertexArrayObject $= Just vao
        GL.vertexAttribPointer (GL.AttribLocation 0)
            $= ( GL.ToFloat
               , GL.VertexArrayDescriptor 3 GL.Float 0 (bufferOffset 0)
               )
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

        loop window

    loop window = do
        shouldClose <- GLFW.windowShouldClose window
        unless shouldClose $ do
            GLFW.pollEvents
            GL.clearColor $= GL.Color4 0.2 0.3 0.3 1.0
            GL.clear [GL.ColorBuffer]
            GL.drawArrays GL.Triangles 0 3
            GLFW.swapBuffers window
            loop window

    vertices :: V.Vector (GL.Vertex3 GL.GLfloat)
    vertices = V.fromList
        [ GL.Vertex3 (-1.0) (-1.0) 0.0
        , GL.Vertex3 1.0 (-1.0) 0.0
        , GL.Vertex3 0.0  1.0 0.0
        ]


    bufferOffset :: Integral a => a -> Ptr b
    bufferOffset = plusPtr nullPtr . fromIntegral

    vertexShaderText :: ByteString
    vertexShaderText =
        "#version 330 core\n\
        \layout (location = 0) in vec3 aPos;\n\
        \void main()\n\
        \{\n\
        \    gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n\
        \}\n"

    fragmentShaderText :: ByteString
    fragmentShaderText =
        "#version 330 core\n\
        \out vec4 FragColor;\n\
        \void main()\n\
        \{\n\
        \    FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n\
        \}\n"

callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
