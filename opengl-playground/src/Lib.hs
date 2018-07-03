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


data ShaderData = ShaderData !(GL.Vertex3 GL.GLfloat) !(GL.Vertex2 GL.GLfloat)

dummyVertex :: GL.Vertex3 GL.GLfloat
dummyVertex = undefined

dummyFloat :: GL.GLfloat
dummyFloat = undefined

instance Storable ShaderData where
    sizeOf ~(ShaderData a b) = sizeOf a + sizeOf b
    alignment ~(ShaderData _ _) = alignment dummyFloat
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
        GLFW.setKeyCallback window (Just callback)
        GLFW.makeContextCurrent (Just window)

        vao <- GL.genObjectName
        GL.bindVertexArrayObject $= Just vao
        triangle <- GL.genObjectName
        GL.bindBuffer GL.ArrayBuffer $= Just triangle
        V.unsafeWith vertices $ \ptr -> do
            let size = fromIntegral
                    (V.length vertices * sizeOf (V.head vertices))
            GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

        GL.vertexAttribPointer (GL.AttribLocation 0)
            $= ( GL.ToFloat
               , GL.VertexArrayDescriptor 3 GL.Float (fromIntegral . sizeOf $ V.head vertices) (bufferOffset 0)
               )
        GL.vertexAttribPointer (GL.AttribLocation 1)
            $= ( GL.ToFloat
               , GL.VertexArrayDescriptor 2 GL.Float (fromIntegral . sizeOf $ V.head vertices) (bufferOffset $ sizeOf dummyVertex)
               )
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
        GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled

        texture <- loadTexture "assets/tankBase.png"
        -- GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
        -- GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')

        prog <- compileProgram vertexShaderText fragmentShaderText

        texUniformLoc <- get $ GL.uniformLocation prog "texture"
        let val = 0 :: GL.GLint
        GL.uniform texUniformLoc $= val
        GL.activeTexture $= GL.TextureUnit 0
        GL.textureBinding GL.Texture2D $= Just texture

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
        [ ShaderData (GL.Vertex3 (0.5) (0.5) 0.0) $ GL.Vertex2 1.0 1.0
        , ShaderData (GL.Vertex3 (0.5) (-0.5) 0.0) $ GL.Vertex2 1.0 0.0
        , ShaderData (GL.Vertex3 (-0.5) (-0.5) 0.0) $ GL.Vertex2 0.0 0.0
        ]

    bufferOffset :: Integral a => a -> Ptr b
    bufferOffset = plusPtr nullPtr . fromIntegral

    vertexShaderText :: ByteString
    vertexShaderText =
        "#version 330 core\n\
        \layout (location = 0) in vec3 aPos;\n\
        \layout (location = 1) in vec2 cordIn;\n\
        \out vec2 cord;\n\
        \void main()\n\
        \{\n\
        \    gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n\
        \    cord = cordIn;\n\
        \}\n"

    fragmentShaderText :: ByteString
    fragmentShaderText =
        "#version 330 core\n\
        \in vec2 cord; \n\
        \uniform sampler2D texture1;\n\
        \out vec4 FragColor;\n\
        \void main()\n\
        \{\n\
        \    FragColor = vec4(texture(texture1, cord).rgb, 1.0f);\n\
        \}\n"

callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
