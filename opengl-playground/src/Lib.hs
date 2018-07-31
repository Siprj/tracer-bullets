{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where

import Control.Monad (unless, when, void)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.StateVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import qualified Data.Vector.Storable as V
import Foreign.Storable
import Foreign.Ptr (Ptr, plusPtr, nullPtr, castPtr)
import System.Exit (exitFailure)
import System.IO (IO, writeFile)

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.BufferObjects as GL

import Texture
import ObjLoader

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

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

data LoadedModel = LoadedModel
    { modelVAO :: GL.VertexArrayObject
    -- ^ Vertex array object
    , verticesBAO :: GL.BufferObject
    -- ^ Buffer array object
    , indicesEAB :: GL.BufferObject
    , indicesSize :: GL.GLint
    }
  deriving (Show)

loadModel :: Model -> IO LoadedModel
loadModel Model{..} = do
    vao <- GL.genObjectName
    GL.bindVertexArrayObject $= Just vao

    vbao <- loadBuffer vertices
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    get GL.errors >>= print

    eao <- loadIndices indices
    get GL.errors >>= print

    pure $ LoadedModel vao vbao eao (fromIntegral $ V.length indices)
  where
    loadBuffer :: V.Vector (GL.Vertex3 GL.GLfloat) -> IO GL.BufferObject
    loadBuffer buffer = do
        bufferName <- GL.genObjectName
        GL.bindBuffer GL.ArrayBuffer $= Just bufferName
        V.unsafeWith buffer $ \ptr -> do
            let size = fromIntegral
                    (V.length buffer * sizeOf (V.head buffer))
            GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
        get GL.errors >>= print
        GL.vertexAttribPointer (GL.AttribLocation 0) $=
            ( GL.ToFloat
            , GL.VertexArrayDescriptor 3 GL.Float
                (fromIntegral . sizeOf $ V.head buffer) (bufferOffset 0)
            )
        get GL.errors >>= print
        pure bufferName

    loadIndices :: V.Vector GL.GLint -> IO GL.BufferObject
    loadIndices buffer = do
        bufferName <- GL.genObjectName
        get GL.errors >>= print
        GL.bindBuffer GL.ElementArrayBuffer $= Just bufferName
        get GL.errors >>= print
        V.unsafeWith buffer $ \ptr -> do
            let size = fromIntegral
                    (V.length buffer * sizeOf (V.head buffer))
            GL.bufferData GL.ElementArrayBuffer $= (size, ptr, GL.StaticDraw)
        get GL.errors >>= print
        pure bufferName

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
        ref <- newIORef (0,0,-2)
        proMatrixRef <- projectionMatrix 90 0.1 1000 1 >>= newIORef
        GLFW.setKeyCallback window (Just $ callback ref)
        GLFW.setScrollCallback window (Just $ callbackScrol ref)
        GLFW.setWindowSizeCallback window
            (Just $ callbackWindowSize proMatrixRef)
        GLFW.makeContextCurrent (Just window)

        lModel <- readModel "assets/hoverTank.obj" >>= loadModel
        print "model loaded"

        prog <- compileProgramFile "shader/vertex.vert" "shader/fragment.frag"
        texUniformLoc <- get $ GL.uniformLocation prog "transform"
        proUniformLoc <- get $ GL.uniformLocation prog "projection"
        rotationUniformLoc <- get $ GL.uniformLocation prog "rotation"
        get GL.errors >>= print
        GL.currentProgram $= Just prog
        get GL.errors >>= print

        loop rotationUniformLoc proUniformLoc proMatrixRef texUniformLoc ref window lModel

    loop rotationUniformLoc proUniformLoc proMatrixRef texUniformLoc ref window model = do
        shouldClose <- GLFW.windowShouldClose window
        unless shouldClose $ do
            GLFW.pollEvents
            GL.depthFunc $= Just GL.Less
            GL.clearColor $= GL.Color4 0.2 0.3 0.3 1.0
            GL.clear [GL.ColorBuffer, GL.DepthBuffer]
            (v1, v2, v3) <- readIORef ref
            transMat <- GL.newMatrix GL.ColumnMajor
                [1,0,0,0, 0,1,0,0, 0,0,1,0, v1,v2,v3,1] :: IO (GL.GLmatrix GL.GLfloat)
            proMatrix <- readIORef proMatrixRef
            time <- fromJust <$> GLFW.getTime
            rotMat <- rotationMatrix (realToFrac time) 0 1 0

            GL.uniform texUniformLoc $= transMat
            GL.uniform proUniformLoc $= proMatrix
            GL.uniform rotationUniformLoc $= rotMat
            GL.bindVertexArrayObject $= Just (modelVAO model)
            GL.drawElements GL.Triangles (indicesSize model) GL.UnsignedInt (bufferOffset 0)
--            get GL.errors >>= print
            GLFW.swapBuffers window
            loop rotationUniformLoc proUniformLoc proMatrixRef texUniformLoc ref window model

crateUnitMatrix :: IO (GL.GLmatrix GL.GLfloat)
crateUnitMatrix =
    GL.newMatrix GL.ColumnMajor [1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1]

-- | `fov` stands for field of view
projectionMatrix
    :: GL.GLfloat
    -> GL.GLfloat
    -> GL.GLfloat
    -> GL.GLfloat
    -> IO (GL.GLmatrix GL.GLfloat)
projectionMatrix fov near far windowRation =
    GL.newMatrix GL.ColumnMajor [s,0,0,0, 0,s',0,0, 0,0,v1,-1, 0,0,v2,0]
  where
    fovRad = (pi * fov) / 180
    tanHalf = tan(fovRad / 2)
    s = 1 / (tanHalf * windowRation)
    s' = 1 / tanHalf
    v1 = far / (near - far)
    v2 = (-(far * near)) / (far - near)

rotationMatrix
    :: GL.GLfloat
    -> GL.GLfloat
    -> GL.GLfloat
    -> GL.GLfloat
    -> IO (GL.GLmatrix GL.GLfloat)
rotationMatrix angle x y z =
    GL.newMatrix GL.ColumnMajor
        [ a11,a21,a31,0
        , a12,a22,a32,0
        , a13,a23,a33,0
        , 0,0,0,1
        ]
  where
      angleCos = cos angle
      angleSin = sin angle
      a11 = angleCos + x * x * (1 - angleCos)
      a21 = x * y * (1 - angleCos) + z * angleSin
      a31 = x * z * (1 - angleCos) - y * angleSin

      a12 = x * y * (1 - angleCos) - z * angleSin
      a22 = angleCos + y * y  * (1 - angleCos)
      a32 = y * z * (1 - angleCos) + x * angleSin

      a13 = x * z * (1 - angleCos) + y * angleSin
      a23 = y * z * (1 - angleCos) - x * angleSin
      a33 = angleCos + z * z * (1 - angleCos)

callbackWindowSize :: IORef (GL.GLmatrix GL.GLfloat) -> GLFW.WindowSizeCallback
callbackWindowSize ref _window w h = do
    print "width: "
    print w
    print "height: "
    print h
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    projectionMatrix 90 0.01 100 (fromIntegral w / fromIntegral h) >>= writeIORef ref

callbackScrol
    :: IORef (GL.GLfloat, GL.GLfloat, GL.GLfloat)
    -> GLFW.ScrollCallback
callbackScrol ref window scrollX scrollY = do
    act <- atomicModifyIORef' ref
        $ \(x, y, z) -> (((x, y, z + ((realToFrac scrollY) / 50)), (x, y, z)))
    print act

callback :: IORef (GL.GLfloat, GL.GLfloat, GL.GLfloat) -> GLFW.KeyCallback
callback ref window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
