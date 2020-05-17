{-# LANGUAGE OverloadedStrings
             , NoMonomorphismRestriction, FlexibleContexts, ViewPatterns ,CPP #-}

module Generate
  ( genTests
  )
where

-- import           Test.DocTest
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Text.Megaparsec -- (Parser,parseMaybe)
import           Text.Megaparsec.Char           ( string
                                                , space
                                                , letterChar
                                                )
--import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Void
import           Data.Either
import           System.Directory
import           System.FilePath
import           Run
import           Location
import           Extract                        ( Module(..) )
import           Parse

#if ! MIN_VERSION_base(4,11,0)
import           Data.Semigroup((<>))
#endif

-- import           System.FilePath.Find
-- "--verbose", 
genTests :: [String] -> [String] -> IO ()
genTests opts files = do
  mdls <- doctest2 $ opts ++ files -- "-DETA"
  -- print mdls
  testAll mdls
  mapM_ testFile mdls

testAll :: [Module a] -> IO ()
testAll mdls =
  let names = map (\mdl -> "DocTest." <> T.pack (moduleName mdl)) mdls
  in  writeModule "test/DocTests.hs"
        . T.unlines
        $ [ "module Main where"
          , "import           Test.Tasty"
          , "import           Test.Tasty.HUnit"
          , T.unlines $ map ("import qualified " <>) names
          , "main = (testGroup \"DocTests\" <$> sequence ["
          <> (T.intercalate "," $ map (<> ".tests") names)
          <> "]) >>= defaultMain"
          ]

testFile :: Module [Located DocTest] -> IO ()
testFile mdl = do
  let
    mdlNameS    = moduleName mdl
    mdlName     = T.pack mdlNameS
    path = T.unpack $ "test/DocTest/" <> T.replace "." "/" mdlName <> ".hs"
    (pre, post) = setup $ moduleSetup mdl
    body =
      T.unlines
        $ [ T.unlines pre
          , "{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}"
          , "module DocTest." <> mdlName <> " where"
          , "import qualified DocTest"
          , "import Test.Tasty(TestTree,testGroup)"
          , "import " <> mdlName
          , T.unlines post
          , "tests :: IO TestTree"
          , "tests = testGroup "
          <> T.pack (show mdlNameS)
          <> " <$>"
          <> " sequence ["
          <> (T.intercalate "," . tabs . map content . concat $ moduleContent
               mdl
             )
          <> "]"
          ]
  writeModule path body
 where
  setup = maybe ([], []) (partitionEithers . map setupLine)

  setupLine (Located _ (Example s [])) = asSetup s
  setupLine _                          = error "setupLine:unexpected input"

  asSetup ('l' : 'e' : 't' : d) = Right $ T.strip (T.pack d)
  asSetup (parseOpts -> Just opts) =
    Left $ "{-# LANGUAGE " <> T.intercalate "," opts <> "#-}"
  asSetup s = Right $ T.pack s

  content (Located l (Example s expected)) = T.unwords
    [ "DocTest.test"
    , T.pack $ show $ show l
    , T.pack $ show $ show expected -- result expected
    , "(DocTest.asPrint("
    , T.pack s
    , "))"
    ]
  content (Located l (Property p)) = T.unwords
    ["DocTest.testProp", T.pack $ show $ show l, T.unwords ["(", T.pack p, ")"]]

  -- result = T.pack . show . map expect
  -- expect (ExpectedLine [LineChunk s]) = T.pack s
  -- expect (WildCardLine) = "WILD"
  -- expect u = error $ "expect: unhandled input: " ++ show u

  tabs = map tab

  tab  = ("  " <>)

writeModule :: FilePath -> T.Text -> IO ()
writeModule path body = do
  createDirectoryIfMissing True (takeDirectory path)
  T.writeFile path body

-- x = parseOpts ":set -XBinaryLiterals  -XOverloadedStrings"
parseOpts :: String -> Maybe [T.Text]
parseOpts = parseMaybe languageOpts

languageOpts :: Parser [T.Text]
languageOpts =
  string ":set" *> many (space *> string "-X" *> (T.pack <$> many letterChar))

type Parser = Parsec Void String
