module Graphics.OBJ.Parser where

import Control.Applicative
import Data.Array
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Monoid
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text

data Vertex = Vertex Double Double Double
  deriving (Show, Read, Eq)

data VertexNormal = VertexNormal Double Double Double
  deriving (Show, Read, Eq)

data TextureCoord = TextureCoord Double Double
  deriving (Show, Read, Eq)

type MTLPath = Maybe FilePath

type FaceVertex = (Vertex, VertexNormal, TextureCoord)

data Face = Face FaceVertex FaceVertex FaceVertex
  deriving (Show, Read, Eq)

data FaceRef = FaceRef Int Int Int Int Int Int Int Int Int
  deriving (Show, Read, Eq)

data Line = Line MTLPath [Vertex] [VertexNormal] [TextureCoord] [FaceRef]
  deriving (Show, Read, Eq)

instance Monoid Line where
  mempty = Line Nothing [] [] [] []
  Line m1 vs1 vns1 vts1 fs1 `mappend` Line m2 vs2 vns2 vts2 fs2 =
    Line (getLast $ Last m1 <> Last m2) (vs1 <> vs2) (vns1 <> vns2) (vts1 <> vts2) (fs1 <> fs2)

data OBJFile =
  OBJFile { mtlPath :: MTLPath
          , faces :: [Face] }
  deriving (Show, Read, Eq)

instance Monoid OBJFile where
  mempty = OBJFile Nothing []
  OBJFile m1 f1 `mappend` OBJFile m2 f2 = OBJFile (getLast $ Last m1 <> Last m2) (f1 <> f2)

tryParseOBJFile :: Text -> Either String OBJFile
tryParseOBJFile = parseOnly parseOBJFile

parseOBJFile :: Parser OBJFile
parseOBJFile = parseOBJLines >>= lineToOBJFile

lineToOBJFile :: Line -> Parser OBJFile
lineToOBJFile (Line m vs vns vts frs) = do
  let va = listArray (1, length vs) vs
  let vna = listArray (1, length vns) vns
  let vta = listArray (1, length vts) vts
  fs <- forM frs $ \(FaceRef v1 vn1 vt1 v2 vn2 vt2 v3 vn3 vt3) ->
    Face <$> ((,,) <$> la va v1 <*> la vna vn1 <*> la vta vt1)
         <*> ((,,) <$> la va v2 <*> la vna vn2 <*> la vta vt2)
         <*> ((,,) <$> la va v3 <*> la vna vn3 <*> la vta vt3)
  return $ OBJFile m fs
  where la = lookupArray

parseMTLFile :: Parser (Maybe FilePath)
parseMTLFile = choice
  [ Just <$> Text.unpack <$> (ss "map_Kd" *> ss (takeTill isEndOfLine))
  , takeTill isEndOfLine *> endOfLine *> pure Nothing ]

lookupArray :: Ix i => Array i e -> i -> Parser e
lookupArray a i =
  if inRange (bounds a) i
    then return $ a ! i
    else mempty

parseOBJLines :: Parser Line
parseOBJLines = mconcat <$> many parseLine

parseLine :: Parser Line
parseLine = choice
  [ parseThing Vertex (\v -> Line Nothing [v] [] [] []) "v"
  , parseThing VertexNormal (\vn -> Line Nothing [] [vn] [] []) "vn"
  , parseTexture
  , parseMTL
  , parseFaceRef
  , parseComment
  , takeTill isEndOfLine *> endOfLine *> pure mempty ]

parseThing :: (Double -> Double -> Double -> a) -> (a -> Line) -> Parser Text -> Parser Line
parseThing f g c = fmap g $ f <$> (ss c *> ss double) <*> ss double <*> ss double

parseTexture :: Parser Line
parseTexture = do
  _ <- ss "vt"
  vt1 <- ss double
  vt2 <- ss double
  return $ Line Nothing [] [] [TextureCoord vt1 vt2] []

parseMTL :: Parser Line
parseMTL = do
  m <- ss "mtllib" *> ss (takeTill isEndOfLine)
  return $ Line (Just $ Text.unpack m) [] [] [] []

parseComment :: Parser Line
parseComment = ss "#" *> ss (takeTill isEndOfLine) *> pure (Line Nothing [] [] [] [])

parseFaceRef :: Parser Line
parseFaceRef = do
  f <- FaceRef <$> (ss "f" *> ss decimal)
               <*> ("/" *> ss decimal)
               <*> ("/" *> ss decimal)
               <*> ss decimal
               <*> ("/" *> ss decimal)
               <*> ("/" *> ss decimal)
               <*> ss decimal
               <*> ("/" *> ss decimal)
               <*> ("/" *> ss decimal)
  return $ Line Nothing [] [] [] [f]

ss :: Parser a -> Parser a
ss m = m <* skipSpace
