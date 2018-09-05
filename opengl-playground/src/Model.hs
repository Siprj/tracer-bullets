{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Model
    ( LoadedModel(..)
    , loadModel
    )
  where

import Prelude ((*), Integral, fromIntegral)
import Foreign.Storable (sizeOf)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Control.Monad ((>>=), (>>))
import Control.Applicative (pure)
import Data.Function (($), (.))
import Data.Vector.Storable (Vector, length, head, unsafeWith)
import Data.Maybe (Maybe(Just))
import Data.StateVar (($=), get)
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.VertexArrayObjects (VertexArrayObject)
import Graphics.Rendering.OpenGL.GL.BufferObjects (BufferTarget(ArrayBuffer))
import Data.ObjectName (genObjectName)
import Graphics.Rendering.OpenGL.GL.VertexArrayObjects (bindVertexArrayObject)
import Graphics.GL.Compatibility30 (GLint, GLfloat)
import Graphics.GL.Functions
import Graphics.GL.Tokens
import Text.Show (Show)
import System.IO (IO, print)

import ObjLoader (Model(..))


data LoadedModel = LoadedModel
    { modelVAO :: VertexArrayObject
    -- ^ Vertex array object
    , verticesBAO :: BufferObject
    -- ^ Buffer array object
    , normalBAO :: BufferObject
    , colorBAO :: BufferObject
    , indicesEAB :: BufferObject
    , indicesSize :: GLint
    }
  deriving (Show)

loadModel :: Model -> IO LoadedModel
loadModel Model{..} = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao

    vbao <- loadBuffer vertices
    vertexAttribArray (AttribLocation 0) $= Enabled
    get errors >>= print

    nbao <- loadNormal vertexNormals
    vertexAttribArray (AttribLocation 1) $= Enabled
    get errors >>= print

    cbao <- loadColor color
    vertexAttribArray (AttribLocation 2) $= Enabled
    get errors >>= print

    eao <- loadIndices indices
    get errors >>= print

    pure $ LoadedModel vao vbao nbao cbao eao (fromIntegral $ length indices)
  where
    loadBuffer :: Vector (Vertex3 GLfloat) -> IO BufferObject
    loadBuffer buffer = do
        bufferName <- genObjectName
        bindBuffer ArrayBuffer $= Just bufferName
        unsafeWith buffer $ \ptr -> do
            let size = fromIntegral
                    (length buffer * sizeOf (head buffer))
            bufferData ArrayBuffer $= (size, ptr, StaticDraw)
        get errors >>= print
        vertexAttribPointer (AttribLocation 0) $=
            ( ToFloat
            , VertexArrayDescriptor 3 Float
                (fromIntegral . sizeOf $ head buffer) (nullPtr)
            )
        get errors >>= print
        pure bufferName

    loadNormal :: Vector (Vertex3 GLfloat) -> IO BufferObject
    loadNormal buffer = do
        bufferName <- genObjectName
        bindBuffer ArrayBuffer $= Just bufferName
        unsafeWith buffer $ \ptr -> do
            let size = fromIntegral
                    (length buffer * sizeOf (head buffer))
            bufferData ArrayBuffer $= (size, ptr, StaticDraw)
        get errors >>= print
        vertexAttribPointer (AttribLocation 1) $=
            ( ToFloat
            , VertexArrayDescriptor 3 Float
                (fromIntegral . sizeOf $ head buffer) nullPtr
            )
        get errors >>= print
        pure bufferName

    loadIndices :: Vector GLint -> IO BufferObject
    loadIndices buffer = do
        bufferName <- genObjectName
        get errors >>= print
        bindBuffer ElementArrayBuffer $= Just bufferName
        get errors >>= print
        unsafeWith buffer $ \ptr -> do
            let size = fromIntegral
                    (length buffer * sizeOf (head buffer))
            bufferData ElementArrayBuffer $= (size, ptr, StaticDraw)
        get errors >>= print
        pure bufferName

    loadColor :: Vector (Color3 GLfloat) -> IO BufferObject
    loadColor buffer = do
        bufferName <- genObjectName
        bindBuffer ArrayBuffer $= Just bufferName
        unsafeWith buffer $ \ptr -> do
            let size = fromIntegral
                    (length buffer * sizeOf (head buffer))
            bufferData ArrayBuffer $= (size, ptr, StaticDraw)
        get errors >>= print
        vertexAttribPointer (AttribLocation 2) $=
            ( ToFloat
            , VertexArrayDescriptor 3 Float
                (fromIntegral . sizeOf $ head buffer) nullPtr
            )
        get errors >>= print
        pure bufferName
