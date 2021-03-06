{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module ObjLoader where

import Prelude ((-), realToFrac, undefined, fromIntegral)

import Control.Applicative ((<*>), (*>), (<*), pure, some, liftA2)
import Control.Applicative.Combinators
import Control.Lens
import Control.Monad ((=<<), (>>=), void, fail, mapM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , anyChar
    , char
    , decimal
    , double
    , endOfInput
    , endOfLine
    , isEndOfLine
    , parseOnly
    , skipSpace
    , skipWhile
    , string
    , takeWhile1
    )
import Data.Bool ((||), not)
import Data.ByteString (readFile)
import Data.ByteString.Char8 (unpack)
import Data.Either (Either(Left, Right), either)
import Data.Eq ((==))
import Data.Foldable (toList)
import Data.Function ((.), ($))
import Data.Functor ((<$>), ($>), fmap)
import Data.Int (Int)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Map.Strict hiding (toList, singleton)
import Data.Monoid (mempty)
import Data.Sequence (Seq, length, singleton)
import Data.String (String)
import Data.Traversable (mapM, sequence)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V
import Graphics.Rendering.OpenGL
    (GLint, GLfloat, Vertex3(Vertex3), Vertex2(Vertex2), Color3(Color3))
import System.IO (IO, FilePath, print)
import System.FilePath.Posix ((</>), takeDirectory)
import Text.Show (Show)

import MtlLoader
    ( Material
        ( Material
        , name
        , ns
        , ka
        , kd
        , ks
        , ke
        , ni
        , d
        )
    , Materials
    , readMaterials
    , materialByName
    )


data Model = Model
    { vertices :: SV.Vector (Vertex3 GLfloat)
    , textureUV :: SV.Vector (Vertex2 GLfloat)
    -- ^ This vector may be empty.
    , vertexNormals :: SV.Vector (Vertex3 GLfloat)
    -- ^ This vector may be empty.
    , indices :: SV.Vector GLint
    , color :: SV.Vector (Color3 GLfloat)
    }
  deriving (Show)

data Face = Face
    { v1 :: (Int, Maybe Int, Maybe Int)
    , v2 :: (Int, Maybe Int, Maybe Int)
    , v3 :: (Int, Maybe Int, Maybe Int)
    }
  deriving (Show)

data MaterialGroup = MaterialGroup
    { _materialName :: Maybe String
    , _faces :: Seq Face
    }
  deriving (Show)

makeLenses ''MaterialGroup

emptyMaterialGroup :: String -> MaterialGroup
emptyMaterialGroup name = MaterialGroup (Just name) mempty

data ObjParserState = ObjParserState
    { _vertices' :: Seq (Vertex3 GLfloat)
    , _textureUV' :: Seq (Vertex2 GLfloat)
    , _vertexNormals' :: Seq (Vertex3 GLfloat)
    , _materialGroups :: Seq MaterialGroup
    , _materialLibrary :: Maybe String
    }
  deriving (Show)

emptyObjParserState :: ObjParserState
emptyObjParserState = ObjParserState
    { _vertices' = mempty
    , _textureUV' = mempty
    , _vertexNormals' = mempty
    , _materialGroups = singleton $ MaterialGroup Nothing mempty
    , _materialLibrary = Nothing
    }

makeLenses ''ObjParserState

type ObjParser = StateT ObjParserState Parser

data ObjConvertState = ObjConvertState
    { _processed :: Map (Int, Maybe Int, Maybe Int) Int
    , _convertedVertices :: Seq (Vertex3 GLfloat)
    , _convertedUV :: Seq (Vertex2 GLfloat)
    , _convertedNormals :: Seq (Vertex3 GLfloat)
    , _convertedColor :: Seq (Color3 GLfloat)
    , _indices' :: Seq Int
    }
  deriving (Show)

emptyObjConvertState :: ObjConvertState
emptyObjConvertState = ObjConvertState
    { _processed = mempty
    , _convertedVertices = mempty
    , _convertedUV = mempty
    , _convertedNormals = mempty
    , _convertedColor = mempty
    , _indices' = mempty
    }

makeLenses ''ObjConvertState

type ConvertorT = State ObjConvertState

readModel :: FilePath -> IO Model
readModel fileName = do
    res <- parseOnly
        (execStateT parseObjLines emptyObjParserState) <$> readFile fileName
    case res of
        Left err -> fail err
        Right res'@ObjParserState{..} -> do
            mtl <- sequence $ readMaterials . materialFileName <$> _materialLibrary
            pure $ convert mtl res'
  where
    materialFileName file = takeDirectory fileName </> file

-- TODO: error handling
convert :: Maybe Materials -> ObjParserState -> Model
convert materials ObjParserState{..} = toModel $ execState
    (mapM convertMaterialGrous _materialGroups) emptyObjConvertState
  where
    convertMaterialGrous :: MaterialGroup -> ConvertorT ()
    convertMaterialGrous MaterialGroup{..} =
        --TODO: This may not work how intended when there is material missing
        -- in material file.
        mapM_ (go $ do
             name' <- _materialName
             materails' <- materials
             materialByName name' materails'
             ) _faces

    go :: Maybe Material -> Face -> ConvertorT ()
    go material Face{..} = do
        goVector material v1
        goVector material v2
        goVector material v3

    goVector :: Maybe Material -> (Int, Maybe Int, Maybe Int) -> ConvertorT ()
    goVector material k@(v, t, n) = do
        ObjConvertState{..} <- get
        case _processed !? k of
            Just i -> modify $ over indices' (|> i)
            Nothing -> do
                let index = length _convertedVertices
                modify $ over indices' (|> index)
                modify $ over convertedVertices (|> vertexVector V.! v)
                maybe (pure ()) (\x -> modify $ over convertedUV
                    (|> textureCoordinateVector V.! x)) t
                maybe (pure ()) (\x -> modify $ over convertedNormals
                    (|> normalVector V.! x)) n
                maybe (pure ()) (\x -> modify $ over convertedColor (|> x))
                    $ fmap getColor material
      where
        getColor Material{..} = kd

    vertexVector :: V.Vector (Vertex3 GLfloat)
    vertexVector = V.fromList $ toList _vertices'

    textureCoordinateVector :: V.Vector (Vertex2 GLfloat)
    textureCoordinateVector = V.fromList $ toList _textureUV'

    normalVector :: V.Vector (Vertex3 GLfloat)
    normalVector = V.fromList $ toList _vertexNormals'

    -- TODO: error handling
    validateFaces :: V.Vector Face -> Either String ()
    validateFaces = undefined

    toModel :: ObjConvertState -> Model
    toModel ObjConvertState{..} = Model
        { vertices = SV.fromList $ toList _convertedVertices
        , textureUV = SV.fromList $ toList _convertedUV
        , vertexNormals = SV.fromList $ toList _convertedNormals
        , indices = SV.fromList . fmap fromIntegral $ toList _indices'
        , color = SV.fromList $ toList _convertedColor
        }

parseObjLines :: ObjParser ()
parseObjLines = do
    void $
        ( parseVertex
        <|> parseTexture
        <|> parseNormal
        <|> parseFace
        <|> parseComment
        <|> parseUnsupported
        <|> parseMtllib
        <|> parseUsemtl
        ) `sepBy` lift (some endOfLine)
    lift $ skipSpace *> endOfInput
  where
    parseVertex :: ObjParser ()
    parseVertex = do
        v <- lift $ do
            string "v" *> skipSpace
            Vertex3 <$> (realToFrac <$> double <* skipSpace)
                <*> (realToFrac <$> double <* skipSpace)
                <*> (realToFrac <$> double)
        modify (over vertices' (|> v))

    parseComment :: ObjParser ()
    parseComment = lift $ string "#" *>
        skipWhile (\c -> not $ c == '\r' || c == '\n')

    parseUnsupported :: ObjParser ()
    parseUnsupported = lift $ (string "g" <|> string "s" <|> string "o") *>
        skipWhile (\c -> not $ c == '\r' || c == '\n')

    parseTexture :: ObjParser ()
    parseTexture = do
        vt <- lift $ do
            string "vt" *> skipSpace
            Vertex2 <$> (realToFrac <$> double <* skipSpace)
                <*> (realToFrac <$> double)
        modify (over textureUV' (|> vt))

    parseMtllib :: ObjParser ()
    parseMtllib = do
        matllib <- lift $ do
            string "mtllib" *> skipSpace
            takeWhile1 (\c -> not $ c == '\r' || c == '\n')
        modify (set materialLibrary (Just $ unpack matllib))

    parseUsemtl :: ObjParser ()
    parseUsemtl = do
        materialName <- lift $ do
            string "usemtl" *> skipSpace
            takeWhile1 (\c -> not $ c == '\r' || c == '\n')
        modify (over materialGroups
            (|> emptyMaterialGroup (unpack materialName)))

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
        modify (over (materialGroups . _last . faces) (|> f))

    parseFacePart :: Parser (Int, Maybe Int, Maybe Int)
    parseFacePart = multiple <|> single
      where
        single = do
            v <- (\v -> v - 1) <$> decimal
            pure (v, Nothing, Nothing)

        multiple = do
            v <- (\v -> v - 1) <$> decimal
            t <- parseOptionalNumber
            n <- parseOptionalNumber
            pure (v, t, n)

    parseOptionalNumber :: Parser (Maybe Int)
    parseOptionalNumber =
        char '/' *> ((Just . (\v -> v - 1) <$> decimal) <|> pure Nothing)
