{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module MtlLoader
    ( Material
    , Materials
    , readMaterials
    )
  where

import Prelude ((-), realToFrac, undefined, fromIntegral)

import Control.Applicative ((<*), (*>), (<*>), (<|>), pure, some)
import Control.Applicative.Combinators (sepBy)
import Control.Monad (fail, mapM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Lens
import Data.Attoparsec.Text
    ( Parser
    , parseOnly
    , anyChar
    , char
    , decimal
    , double
    , endOfInput
    , endOfLine
    , isEndOfLine
    , skipSpace
    , skipWhile
    , string
    , takeWhile1
    , many'
    )
import Data.Bool ((||), not)
import Data.Char (isAlphaNum)
import Data.Either (Either(Left, Right), either)
import Data.Eq ((==))
import Data.Function (($), (.), id)
import Data.Functor ((<$>), fmap, void)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (String)
import Graphics.Rendering.OpenGL
    (Color3(Color3), GLfloat, Vertex3(Vertex3), Vertex2(Vertex2))
import Data.Text (Text, unpack)
import Data.Text.IO (readFile)
import Text.Show (Show)
import System.IO (IO, FilePath)


data Material = Material
    { name :: !String
    , ns :: !GLfloat
    -- ^ Specular exponent.
    , ka :: Color3 GLfloat
    -- ^ Ambient colour.
    , kd :: Color3 GLfloat
    -- ^ Difuse colour.
    , ks :: Color3 GLfloat
    -- ^ Specular colour.
    , ke :: Maybe (Color3 GLfloat)
    -- ^ Emission coefficient.
    , ni :: Maybe GLfloat
    -- ^ Optical density/index of refraction. Can contain value in range 0.001
    -- to 1.0.
    , d :: !GLfloat
    -- ^ Transparency 1.0 fully opaque, 0 is fully dissolved.
-- TODO: Maps
--    , map_Ka :: !Maybe FilePath
--    , map_Kd :: !Maybe FilePath
--    , map_Kd :: !Maybe FilePath
    }
  deriving (Show)

type Materials = [Material]

data MaterialState = MaterialState
    { _materialStateName :: !String
    , _materialStateNs :: Maybe GLfloat
    -- ^ Specular exponent.
    , _materialStateKa :: Maybe (Color3 GLfloat)
    -- ^ Ambient colour.
    , _materialStateKd :: Maybe (Color3 GLfloat)
    -- ^ Difuse colour.
    , _materialStateKs :: Maybe (Color3 GLfloat)
    -- ^ Specular colour.
    , _materialStateKe :: Maybe (Color3 GLfloat)
    -- ^ Emission coefficient.
    , _materialStateNi :: Maybe GLfloat
    -- ^ Optical density/index of refraction. Can contain value in range 0.001
    -- to 1.0.
    , _materialStateD :: Maybe GLfloat
    -- ^ Transparency 1.0 fully opaque, 0 is fully dissolved.
-- TODO: Maps
--    , _materialStateMap_Ka :: Maybe FilePath
--    , _materialStateMap_Kd :: Maybe FilePath
--    , _materialStateMap_Ke :: Maybe FilePath
    }
  deriving (Show)

makeEmptyMaterial :: String -> MaterialState
makeEmptyMaterial name = MaterialState
    { _materialStateName = name
    , _materialStateNs = Nothing
    , _materialStateKa = Nothing
    , _materialStateKd = Nothing
    , _materialStateKs = Nothing
    , _materialStateKe = Nothing
    , _materialStateNi = Nothing
    , _materialStateD = Nothing
--    , _materialStateMap_Ka = Nothing
--    , _materialStateMap_Kd = Nothing
--    , _materialStateMap_Ke = Nothing
    }

makeLenses ''MaterialState

type MaterialParser = StateT [MaterialState] Parser

readMaterials :: FilePath -> IO Materials
readMaterials fileName = do
    res <- parseOnly (execStateT parseMaterials [])
        <$> readFile fileName
    either fail convert res

convert :: [MaterialState] -> IO Materials
convert = maybe err pure . mapM convert'
  where
    convert' :: MaterialState -> Maybe Material
    convert' MaterialState{..} = do
        ns <- _materialStateNs
        ka <- _materialStateKa
        kd <- _materialStateKd
        ks <- _materialStateKs
        d <- _materialStateD
        pure $ Material
            { name =_materialStateName
            , ke = _materialStateKe
            , ni = _materialStateNi
            , ..
            }
    err = fail "Every material must contain Ns, Ka, Kd, Ks, d fileds."

parseMaterials :: MaterialParser ()
parseMaterials = do
    parseFirstName
    void $ many'
        ( parseMaterialName
        <|> parseNs
        <|> parseKa
        <|> parseKd
        <|> parseKs
        <|> parseKe
        <|> parseNi
        <|> parseD
        <|> parseComment
        <|> parseUnsupported
        ) `sepBy` lift (some endOfLine)

parseMaterialName :: MaterialParser ()
parseMaterialName = do
    name <- lift $ do
        string "newmtl" *> skipSpace
        fmap unpack (takeWhile1 isValidNameChar) <* skipSpace
    modify (makeEmptyMaterial name <|)

isValidNameChar c = isAlphaNum c || c == '_' || c == '.'

parseFirstName :: MaterialParser ()
parseFirstName = lift skipSpace *>  (parseComment `sepBy` lift skipSpace) *>
    lift skipSpace *> parseMaterialName

parseNs :: MaterialParser ()
parseNs = parseModifyHead (parseFloat "Ns") materialStateNs

parseKa :: MaterialParser ()
parseKa = parseModifyHead (parseColor "Ka") materialStateKa

parseKd :: MaterialParser ()
parseKd = parseModifyHead (parseColor "Kd") materialStateKd

parseKs :: MaterialParser ()
parseKs = parseModifyHead (parseColor "Ks") materialStateKs

parseKe :: MaterialParser ()
parseKe = parseModifyHead (parseColor "Ke") materialStateKe

parseNi :: MaterialParser ()
parseNi = parseModifyHead (parseFloat "Ni") materialStateNi

parseD :: MaterialParser ()
parseD = parseModifyHead (parseFloat "d") materialStateD

-- TODO: Support map_Kd.
parseUnsupported :: MaterialParser ()
parseUnsupported = lift $ (string "illum" <|> string "map_Kd")
    *> skipWhile (not . isEndOfLine)

parseModifyHead
    :: (MaterialParser a)
    -> Lens' MaterialState (Maybe a)
    -> MaterialParser ()
parseModifyHead p f = do
    v <- p
    modify (set (_head . f) $ Just v)

parseFloat :: Text -> MaterialParser GLfloat
parseFloat fieldName = lift $ do
    string fieldName *> skipSpace
    realToFrac <$> double

parseColor :: Text -> MaterialParser (Color3 GLfloat)
parseColor colorName = lift $ do
    string colorName *> skipSpace
    c1 <- (realToFrac <$> double) <* skipSpace
    c2 <- (realToFrac <$> double) <* skipSpace
    c3 <- (realToFrac <$> double)
    pure $ Color3 c1 c2 c3

parseComment :: MaterialParser ()
parseComment = lift $ string "#" *>
    skipWhile (not . isEndOfLine)
