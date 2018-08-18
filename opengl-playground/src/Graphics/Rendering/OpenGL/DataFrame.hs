{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

module Graphics.Rendering.OpenGL.DataFrame where

import GHC.Base (IO(..))
import GHC.Exts (unsafeCoerce#, unsafeFreezeByteArray#, byteArrayContents#, newAlignedPinnedByteArray#, runRW#, plusAddr#, isByteArrayPinned#, isTrue#)
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (..), unsafeForeignPtrToPtr)
import Prelude (undefined, fromIntegral, Float)

import Data.Function (($))
import Data.StateVar (makeStateVar)
import Foreign.Ptr (Ptr, plusPtr, nullPtr, castPtr)
import Foreign.Ptr (Ptr, plusPtr, nullPtr, castPtr)
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform (Uniform, uniformv, uniform)
import Graphics.Rendering.OpenGL (UniformLocation(UniformLocation), GLint, GLfloat)
import Graphics.GL.Functions (glUniformMatrix3fv, glUniformMatrix4fv)
import Graphics.GL.Tokens (pattern GL_FALSE)
import Numeric.Dimensions (Evidence(E))
import Numeric.DataFrame (Mat44f, Mat33f, Matrix, inferPrim)
import Numeric.DataFrame.Internal.Array.Family (ArraySing(ABase), aSing)
import Numeric.PrimBytes (byteSize, byteOffset, getBytes, byteAlign, writeBytes)


instance Uniform (Matrix GLfloat 3 3) where
    uniform (UniformLocation ul) = makeStateVar getter setter
      where
        getter = undefined
        setter x = glUniformMatrix3fv (fromIntegral ul :: GLint) 1 GL_FALSE $
            case aSing @GLfloat @[3, 3] of
                ABase
                  | ba <- getBytes x
                  , isTrue# (isByteArrayPinned# ba)
                  -> unsafeForeignPtrToPtr $ ForeignPtr
                    (plusAddr# (byteArrayContents# ba) (byteOffset x))
                    (PlainPtr (unsafeCoerce# ba))

                _ | E <- inferPrim x -> case runRW#
                    ( \s0 -> case newAlignedPinnedByteArray#
                                    (byteSize @Mat33f undefined)
                                    (byteAlign @Mat33f undefined) s0 of
                      (# s1, mba #) ->
                          unsafeFreezeByteArray# mba (writeBytes mba 0# x s1)
                    ) of
                    (# _, ba #) -> unsafeForeignPtrToPtr
                        $ ForeignPtr (byteArrayContents# ba)
                        (PlainPtr (unsafeCoerce# ba))
    uniformv loc = undefined

instance Uniform (Matrix GLfloat 4 4) where
    uniform (UniformLocation ul) = makeStateVar getter setter
      where
        getter = undefined
        setter x = glUniformMatrix4fv (fromIntegral ul :: GLint) 1 GL_FALSE $
            case aSing @Float @[4, 4] of
                ABase
                  | ba <- getBytes x
                  , isTrue# (isByteArrayPinned# ba)
                  -> unsafeForeignPtrToPtr $ ForeignPtr
                    (plusAddr# (byteArrayContents# ba) (byteOffset x))
                    (PlainPtr (unsafeCoerce# ba))

                _ | E <- inferPrim x -> case runRW#
                    ( \s0 -> case newAlignedPinnedByteArray#
                                    (byteSize @Mat44f undefined)
                                    (byteAlign @Mat44f undefined) s0 of
                      (# s1, mba #) ->
                          unsafeFreezeByteArray# mba (writeBytes mba 0# x s1)
                    ) of
                    (# _, ba #) -> unsafeForeignPtrToPtr
                        $ ForeignPtr (byteArrayContents# ba)
                        (PlainPtr (unsafeCoerce# ba))
    uniformv loc = undefined
