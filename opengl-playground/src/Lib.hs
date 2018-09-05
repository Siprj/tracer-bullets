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
        ref <- newIORef (0,0,-20)
        -- proMatrixRef <- projectionMatrix 90 0.1 100 1 >>= newIORef
        let p = perspective 0.1 100 ((pi * 90) / 180) 1 :: Mat44f
        proMatrixRef <- newIORef $ p
        print p
        projectionMatrix 90 0.1 100 1
        GLFW.setKeyCallback window (Just $ callback ref)
        GLFW.setScrollCallback window (Just $ callbackScrol ref)
        GLFW.setWindowSizeCallback window
            (Just $ callbackWindowSize proMatrixRef)
        GLFW.makeContextCurrent (Just window)

        lModel <- readModel "assets/hoverTank2.obj" >>= loadModel
        print "model loaded"

        prog@OpaqueShader{..} <- compileOpaqueShader
        setup
        get GL.errors >>= print

        loop prog proMatrixRef ref window lModel

    loop prog@OpaqueShader{..} proMatrixRef ref window model = do
        shouldClose <- GLFW.windowShouldClose window
        unless shouldClose $ do
            GLFW.pollEvents
            GL.depthFunc $= Just GL.Less
            GL.cullFace $= Just GL.Back
            GL.clearColor $= GL.Color4 0.2 0.3 0.3 1.0
            GL.clear [GL.ColorBuffer, GL.DepthBuffer]
            (v1, v2, v3) <- readIORef ref
            time <- realToFrac . fromJust <$> GLFW.getTime
            let modelMatrix = translate3 (vec3 v1 v2 v3) %* rotateY (time / 10)
            proMatrix <- readIORef proMatrixRef

            setProjectionMatrix proMatrix
            setModelMatrix modelMatrix
            setViewMatrix eye
            setLightPosition $ GL.Vector3 10 10 10
            GL.bindVertexArrayObject $= Just (modelVAO model)
            GL.drawElements GL.Triangles (indicesSize model) GL.UnsignedInt nullPtr
            GLFW.swapBuffers window
            loop prog proMatrixRef ref window model

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
    :: IORef (GL.GLfloat, GL.GLfloat, GL.GLfloat)
    -> GLFW.ScrollCallback
callbackScrol ref window scrollX scrollY = do
    act <- atomicModifyIORef' ref
        $ \(x, y, z) -> ((x, y, z + realToFrac scrollY), (x, y, z))
    print act

callback :: IORef (GL.GLfloat, GL.GLfloat, GL.GLfloat) -> GLFW.KeyCallback
callback ref window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)
