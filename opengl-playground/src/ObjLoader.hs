{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module ObjLoader where

import Prelude (realToFrac, undefined, fromIntegral)

import Control.Applicative ((<*>), (*>), (<*), pure, some)
import Control.Applicative.Combinators
import Control.Lens
import Control.Monad ((=<<), void)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.Bool ((||), not)
import Data.Either (Either)
import Data.Eq ((==))
import Data.Foldable (toList)
import Data.Function ((.), ($))
import Data.Functor ((<$>), ($>))
import Data.Int (Int)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Map.Strict hiding (toList)
import Data.Sequence (Seq)
import Data.String (String)
import Data.Traversable (mapM)
import Graphics.Rendering.OpenGL
    (GLint, GLfloat, Vertex3(Vertex3), Vertex2(Vertex2))
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V
import System.IO (IO, FilePath)
import Data.Attoparsec.ByteString (Parser)
import Text.Show (Show)
import Data.Attoparsec.ByteString.Char8
    ( anyChar
    , char
    , decimal
    , double
    , endOfInput
    , endOfLine
    , isEndOfLine
    , skipSpace
    , skipWhile
    , string
    )

data Model = Model
    { vertices :: SV.Vector (Vertex3 GLfloat)
    -- | This vector may be empty.
    , textureUV :: SV.Vector (Vertex2 GLfloat)
    -- | This vector may be empty.
    , vertexNormals :: SV.Vector (Vertex3 GLfloat)
    , indices :: SV.Vector GLint
    }
  deriving (Show)

data ObjParserState = ObjParserState
    { _vertices' :: Seq (Vertex3 GLfloat)
    , _textureUV' :: Seq (Vertex2 GLfloat)
    , _vertexNormals' :: Seq (Vertex3 GLfloat)
    , _faces' :: Seq Face
    }
  deriving (Show)

data Face = Face
    { v1 :: (Int, Maybe Int, Maybe Int)
    , v2 :: (Int, Maybe Int, Maybe Int)
    , v3 :: (Int, Maybe Int, Maybe Int)
    }
  deriving (Show)

makeLenses ''ObjParserState

type ObjParser = StateT ObjParserState Parser

data ObjConvertState = ObjConvertState
    { _processed :: Map (Int, Maybe Int, Maybe Int) Int
    , _convertedVertices :: Seq (Vertex3 GLfloat)
    , _convertedUV :: Seq (Vertex2 GLfloat)
    , _convertedNormals :: Seq (Vertex3 GLfloat)
    , _indices' :: Seq (Int)
    }
  deriving (Show)

makeLenses ''ObjConvertState

type ConvertorT = State ObjConvertState

loadModel :: FilePath -> IO Model
loadModel fileName = undefined

convert :: ObjState -> Either String Model
convert ObjState{..} = undefined
  where
    go :: Face -> ConvertorT ()
    go Face{..} = undefined

    goVector :: (Int, Maybe Int, Maybe Int) -> ConvertorT ()
    goVector k = do
        ObjConvertState{..} <- get
        case _processed !? k of
            Just i -> modify $ over indices' |> i
            Nothing ->

    vertexVector :: V.Vector (Vertex3 GLfloat)
    vertexVector = V.fromList $ toList _vertices'

    textureCoordinateVector :: V.Vector (Vertex2 GLfloat)
    textureCoordinateVector = V.fromList $ toList _textureUV'

    normalVector :: V.Vector (Vertex3 GLfloat)
    normalVector = V.fromList $ toList _vertexNormals'

    faceVector :: V.Vector Face
    faceVector = V.fromList $ toList _faces'

    validateFaces :: V.Vector Face -> Either String ()
    validateFaces = undefined


parseObjLines :: ObjParser ()
parseObjLines = void $
    ( parseVertex
    <|> parseTexture
    <|> parseNormal
    <|> parseFace
    <|> parseComment
    <|> parseGroup
    ) `sepBy` lift (some endOfLine)
  where
    parseVertex :: ObjParser ()
    parseVertex = do
        v <- lift $ do
            string "v" *> skipSpace
            Vertex3 <$> (realToFrac <$> double <* skipSpace)
                <*> (realToFrac <$> double <* skipSpace)
                <*> (realToFrac <$> double)
        modify (over vertices' (|> v))

    parseComment ::ObjParser ()
    parseComment = lift $ string "#" *>
        skipWhile (\c -> not $ c == '\r' || c == '\n')

    parseGroup :: ObjParser ()
    parseGroup = lift $ (string "g" <|> string "s") *>
        skipWhile (\c -> not $ c == '\r' || c == '\n')

    parseTexture :: ObjParser ()
    parseTexture = do
        vt <- lift $ do
            string "vt" *> skipSpace
            Vertex2 <$> (realToFrac <$> double <* skipSpace)
                <*> (realToFrac <$> double)
        modify (over textureUV' (|> vt))

    parseNormal :: ObjParser ()
    parseNormal = do
        vn <- lift $ do
            string "vn" *> skipSpace
            Vertex3 <$> (realToFrac <$> double <* skipSpace)
                <*> (realToFrac <$> double <* skipSpace)
                <*> (realToFrac <$> double)
        modify (over vertexNormals' (|> vn))

    parseFace :: ObjParser ()
    parseFace = do
        f <- lift $ do
            string "f" *> skipSpace
            v1 <- parseFacePart <* skipSpace
            v2 <- parseFacePart <* skipSpace
            v3 <- parseFacePart
            pure $ Face v1 v2 v3
        modify (over faces' (|> f))

    parseFacePart :: Parser (Int, Maybe Int, Maybe Int)
    parseFacePart = do
        v <- decimal
        t <- parseOptionalNumber
        n <- parseOptionalNumber
        pure (v, t, n)

    parseOptionalNumber :: Parser (Maybe Int)
    parseOptionalNumber =
        char '/' *> ((Just <$> decimal) <|> pure Nothing)
