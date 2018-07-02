{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Monad (unless, when, void)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.StateVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import qualified Data.Vector.Storable as V
import Foreign.Storable
import Foreign.Ptr (Ptr, plusPtr, nullPtr, castPtr)
import System.Exit (exitFailure)
import System.IO (IO)

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.BufferObjects as GL

import Texture


data ShaderData = ShaderData !(GL.Vertex3 GL.GLfloat) !GL.GLfloat

dummyVertex :: GL.Vertex3 GL.GLfloat
dummyVertex = undefined

instance Storable ShaderData where
    sizeOf ~(ShaderData a b) = sizeOf a + sizeOf b
    alignment ~(ShaderData _ x) = alignment x
    peek ptr = ShaderData
        <$> peek (castPtr ptr)
        <*> peek (plusPtr ptr $ sizeOf dummyVertex)
    poke ptr (ShaderData a b) = do
        poke (castPtr ptr) a
        poke (plusPtr ptr $ sizeOf dummyVertex) b

reportError :: String -> Bool -> IO ()
reportError str res = unless res $ putStrLn str >> exitFailure

compileProgramFile :: FilePath -> FilePath -> IO GL.Program
compileProgramFile vertexShaderFileName fragmenShaderFileName = do
    vertexShaderText <- B.readFile vertexShaderFileName
    fragmenShaderText <- B.readFile fragmenShaderFileName

    compileProgram vertexShaderText fragmenShaderText

compileProgram :: ByteString -> ByteString -> IO GL.Program
compileProgram vertexShaderText fragmentShaderText = do
    vertexShader <- GL.createShader GL.VertexShader
    GL.shaderSourceBS vertexShader $= vertexShaderText
    GL.compileShader vertexShader
    get (GL.shaderInfoLog vertexShader) >>= putStrLn
    GL.compileStatus vertexShader
        >>= reportError "Vertec shader compilation FAILED."

    fragmentShader <- GL.createShader GL.FragmentShader
    GL.shaderSourceBS fragmentShader $= fragmentShaderText
    GL.compileShader fragmentShader
    get (GL.shaderInfoLog fragmentShader) >>= putStrLn
    GL.compileStatus fragmentShader
        >>= reportError "Fragmen shader compilation FAILED."

    shaderProgram <- GL.createProgram
    GL.attachShader shaderProgram vertexShader
    GL.attachShader shaderProgram fragmentShader
    GL.linkProgram shaderProgram
    get (GL.programInfoLog shaderProgram) >>= putStrLn
    GL.linkStatus shaderProgram
        >>= reportError "Shader program compilation FAILED."
    GL.currentProgram $= Just shaderProgram

    GL.deleteObjectName vertexShader
    GL.deleteObjectName fragmentShader
    pure shaderProgram

someFunc :: IO ()
someFunc = do
    GLFW.init
    primaryMonitor <- fromJust <$> GLFW.getPrimaryMonitor
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
        putStrLn $ "sizeof " <> (show . sizeOf $ ShaderData (GL.Vertex3 1.0 (-1.0) 0.0) 1.0)
        putStrLn $ "alignment " <> (show . alignment $ ShaderData (GL.Vertex3 1.0 (-1.0) 0.0) 1.0)
        GLFW.setKeyCallback window (Just callback)
        GLFW.makeContextCurrent (Just window)

        triangle <- GL.genObjectName
        GL.bindBuffer GL.ArrayBuffer $= Just triangle
        V.unsafeWith vertices $ \ptr -> do
            let size = fromIntegral
                    (V.length vertices * sizeOf (V.head vertices))
            GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

        vao <- GL.genObjectName
        GL.bindVertexArrayObject $= Just vao
        GL.vertexAttribPointer (GL.AttribLocation 0)
            $= ( GL.ToFloat
               , GL.VertexArrayDescriptor 3 GL.Float (fromIntegral . sizeOf $ V.head vertices) (bufferOffset 0)
               )
        GL.vertexAttribPointer (GL.AttribLocation 1)
            $= ( GL.ToFloat
               , GL.VertexArrayDescriptor 1 GL.Float (fromIntegral . sizeOf $ V.head vertices) (bufferOffset $ sizeOf dummyVertex)
               )
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
        GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled

        texture <- loadTexture "assets/M-6_preview.png"
        compileProgram vertexShaderText fragmentShaderText

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

    vertices :: V.Vector ShaderData
    vertices = V.fromList
        [ ShaderData (GL.Vertex3 (-1.0) (-1.0) 0.0) 1.0
        , ShaderData (GL.Vertex3 1.0 (-1.0) 0.0) 0.1
        , ShaderData (GL.Vertex3 0.0  1.0 0.0) 0.5
        ]

    bufferOffset :: Integral a => a -> Ptr b
    bufferOffset = plusPtr nullPtr . fromIntegral

    vertexShaderText :: ByteString
    vertexShaderText =
        "#version 330 core\n\
        \layout (location = 0) in vec3 aPos;\n\
        \layout (location = 1) in float colorMod;\n\
        \out float color;\n\
        \void main()\n\
        \{\n\
        \    gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n\
        \    color = colorMod;\n\
        \}\n"

    fragmentShaderText :: ByteString
    fragmentShaderText =
        "#version 330 core\n\
        \in float color; \n\
        \out vec4 FragColor;\n\
        \void main()\n\
        \{\n\
        \    FragColor = vec4(1.0f, 0.5f, color, 1.0f);\n\
        \}\n"

callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
