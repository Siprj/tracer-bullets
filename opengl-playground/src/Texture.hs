{-# LANGUAGE RecordWildCards #-}

module Texture where

import qualified Data.Vector.Storable as V
import Codec.Picture
import Data.StateVar
import qualified Graphics.Rendering.OpenGL as GL

loadTexture :: FilePath -> IO GL.TextureObject
loadTexture file = do
    Image{..} <- readImage file >>= either fail (pure . convertRGB8)
    texture <- GL.genObjectName
    GL.textureBinding GL.Texture2D $= Just texture
    V.unsafeWith imageData $ \ptr ->
        GL.texImage2D
            GL.Texture2D
            GL.NoProxy
            0
            GL.RGB8
            (GL.TextureSize2D (fromIntegral imageWidth) (fromIntegral imageHeight))
            0
            (GL.PixelData GL.RGB GL.UnsignedByte ptr)
    pure texture


