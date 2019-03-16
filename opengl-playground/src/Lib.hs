{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnboxedSums               #-}
{-# LANGUAGE UnboxedTuples             #-}

module Lib
    ( someFunc
    ) where

import Control.Monad (unless, when, void)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State.Strict as ST
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef, modifyIORef)
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

import Numeric.Dimensions
import Numeric.DataFrame
import Numeric.DataFrame.Internal.Array.Family hiding (inferPrim)
import Numeric.Matrix
import Numeric.Matrix.Class
import Numeric.PrimBytes
import GHC.Exts
import GHC.Base (IO(..))
import GHC.Ptr (Ptr(..))
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (..), unsafeForeignPtrToPtr)


import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.BufferObjects as GL
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import Graphics.GL.Functions
import Graphics.GL.Tokens

import Texture
import ObjLoader
import Graphics.Rendering.OpenGL.GL.DataFrame
import Model (LoadedModel(..), loadModel)
import Shader.OpaqueShader


data ShaderData = ShaderData !(GL.Vertex3 GL.GLfloat) !(GL.Vertex2 GL.GLfloat)

data State = State
    { cameraPitchRef :: IORef Float
    , cameraYawRef :: IORef Float
    , cameraPositionRef :: IORef Vec3f
    , opaqueShader :: OpaqueShader
    , projectionMatrixRef :: IORef Mat44f
    , loadedModel :: LoadedModel
    , gameWindow :: GLFW.Window
    }

type GameLoopMonad = ST.StateT State IO

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

toRad :: Float -> Float
toRad v = v * (pi/180)

someFunc :: IO ()
someFunc = do
    GLFW.init
    primaryMonitor <- fromJust <$> GLFW.getPrimaryMonitor
    supportedVideoModes <- GLFW.getVideoModes primaryMonitor
    let vm@(GLFW.VideoMode width height red green blue refreshRate) =
            maximum $ maximum supportedVideoModes
    GLFW.windowHint $ GLFW.WindowHint'RedBits $ Just red
    GLFW.windowHint $ GLFW.WindowHint'GreenBits $ Just green
    GLFW.windowHint $ GLFW.WindowHint'BlueBits $ Just blue
    GLFW.windowHint $ GLFW.WindowHint'RefreshRate $ Just refreshRate
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    window <- GLFW.createWindow width height
        "blabla" (Just primaryMonitor) Nothing
    maybe printErrorAndFail (setAndloop primaryMonitor vm) window
    GLFW.terminate
  where
    printErrorAndFail = print "Can't create window" >> exitFailure

    setAndloop primaryMonitor vm window = do
        GLFW.setFullscreen window primaryMonitor vm
        GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
        pitch <- newIORef 0
        yaw <- newIORef 0
        pos <- GLFW.getCursorPos window
        lastMousePos <- newIORef pos
        position <- newIORef (vec3 0 0 (-20))
        let p = perspective 0.1 100 ((pi * 90) / 180) 1 :: Mat44f
        proMatrixRef <- newIORef $ p
        GLFW.setKeyCallback window (Just $ callback pitch yaw position)
        GLFW.setScrollCallback window (Just $ callbackScrol pitch yaw position)
        GLFW.setWindowSizeCallback window
            (Just $ callbackWindowSize proMatrixRef)
        GLFW.setCursorPosCallback window (Just $ mouseCallback lastMousePos yaw pitch)
        GLFW.makeContextCurrent (Just window)

        lModel <- readModel "assets/hoverTank2.obj" >>= loadModel
        print "model loaded"

        prog@OpaqueShader{..} <- compileOpaqueShader
        setup
        get GL.errors >>= print

        ST.runStateT loop $ State pitch yaw position prog proMatrixRef lModel window


    loop :: GameLoopMonad ()
    loop = do
        State{..} <- ST.get
        shouldClose <- liftIO $ GLFW.windowShouldClose gameWindow
        unless shouldClose $ do
            liftIO $ GLFW.pollEvents
            liftIO $ GL.depthFunc $= Just GL.Less
            liftIO $ GL.cullFace $= Just GL.Back
            liftIO $ GL.clearColor $= GL.Color4 0.2 0.3 0.3 1.0
            liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
            time <- liftIO $ realToFrac . fromJust <$> GLFW.getTime
            let modelMatrix = translate3 (vec3 0 0 (-20)) %* rotateY (time / 10)
            proMatrix <- liftIO $ readIORef projectionMatrixRef
            cameraPosition <- liftIO $ readIORef cameraPositionRef
            cameraPitch <- fmap toRad . liftIO $ readIORef cameraPitchRef
            cameraYaw <- fmap toRad . liftIO $ readIORef cameraYawRef
            wPressed <- liftIO $ isPressed <$> GLFW.getKey gameWindow GLFW.Key'W

            let cameraDirection = vec3
                    (cos cameraYaw * cos cameraPitch)
                    (sin cameraPitch)
                    (sin cameraYaw * cos cameraPitch)
            let viewMatrix = lookAt
                    (vec3 0.0 1.0 0.0)
                    cameraPosition
                    (cameraPosition + cameraDirection)
            let OpaqueShader{..} = opaqueShader

            when wPressed . liftIO $ modifyIORef cameraPositionRef
                (\v -> v - cameraDirection * (realToFrac 0.01))


            liftIO $ setProjectionMatrix proMatrix
            liftIO $ setModelMatrix modelMatrix
            liftIO $ setViewMatrix viewMatrix
            liftIO . setLightPosition $ GL.Vector3 10 10 10
            liftIO $ GL.bindVertexArrayObject $= Just (modelVAO loadedModel)
            liftIO $ GL.drawElements GL.Triangles (indicesSize loadedModel) GL.UnsignedInt nullPtr
            liftIO $ GLFW.swapBuffers gameWindow
            loop
        pure ()

isPressed :: GLFW.KeyState -> Bool
isPressed GLFW.KeyState'Pressed = True
isPressed _ = False

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
    GL.newMatrix GL.ColumnMajor ll
  where
    ll = [ s,0,0,0
        , 0,s',0,0
        , 0,0,v1,-1
        , 0,0,v2,0
        ]
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

callbackWindowSize :: IORef Mat44f -> GLFW.WindowSizeCallback
callbackWindowSize ref _window w h = do
    print "width: "
    print w
    print "height: "
    print h
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    writeIORef ref $ perspective 0.1 100 ((pi * 90) / 180) (fromIntegral w / fromIntegral h)

callbackScrol
    :: IORef Float -> IORef Float -> IORef Vec3f
    -> GLFW.ScrollCallback
callbackScrol pitch yaw pos window scrollX scrollY = do
    cameraPosition <- readIORef pos
    cameraPitch <- fmap toRad $ readIORef pitch
    cameraYaw <- fmap toRad $ readIORef yaw
    let cameraDirection = vec3
            (cos cameraYaw * cos cameraPitch)
            (sin cameraPitch)
            (sin cameraYaw * cos cameraPitch)
    modifyIORef pos (\v -> v - cameraDirection * (realToFrac scrollY / 5))

mouseCallback
    :: IORef (Double, Double)
    -> IORef Float
    -> IORef Float
    -> GLFW.CursorPosCallback
mouseCallback lastPos yaw pitch _ x y = do
    (lastX, lastY) <- readIORef lastPos
    let xOffset = realToFrac $ x - lastX
    let yOffset = realToFrac $ y - lastY
    writeIORef lastPos (x, y)
    modifyIORef yaw (+ (xOffset*0.1))
    modifyIORef pitch (\v -> min 89 $ max (-89) $ v + (yOffset*0.1))

callback :: IORef Float -> IORef Float -> IORef Vec3f -> GLFW.KeyCallback
callback pitch yaw pos window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
