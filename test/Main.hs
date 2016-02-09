import           Data.Maybe                     (Maybe (..))
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (path)
import           Test.HUnit.Base                (Assertion)
-- modules we will test
import           M3U.Common                     (EXTINF (..), M3U (..),
                                                 M3UEntry (..))
import           M3U.Read                       (fromFile)
import           M3U.Write                      (toOrderedMarkdown, toYAML)

-- the first couple items here are hand constructed data used for testing
-- purposes.

-- data EXTINF = EXTINF { seconds ∷ Int , artist ∷ String , title ∷ String }
testEXTINF :: EXTINF
testEXTINF = EXTINF { seconds = 60
                    , artist = "testartist1"
                    , title = "testtitle1"
                    }

-- data M3U = M3U { entries ∷ [M3UEntry] } deriving (Eq)
testM3U :: M3U
testM3U = M3U { entries = [ M3UEntry { info = Nothing
                                     , path = "testpath1"
                                     }
                          , M3UEntry { info = Just testEXTINF
                                     , path = "testpath2"
                                     }
                          , M3UEntry { info = Nothing
                                     , path = "testpath3"
                                     }
                          ]
              }

yamlExpected :: String
yamlExpected = "- path: testpath1\n- path: testpath2\n  title: testtitle1\n  artist: testartist1\n- path: testpath3\n"

toYAMLTest :: Assertion
toYAMLTest = assertEqual "toYAMLPrefix" yamlExpected (toYAML testM3U)

main :: IO ()
main = defaultMainWithOpts [ testCase "toYAML" toYAMLTest ] mempty
