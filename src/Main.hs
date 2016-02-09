{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import           Options.Applicative
import           Prelude.Unicode

-- my modules
import           M3U.Read            (fromFile)
import           M3U.Write           (toOrderedMarkdown, toYAML)

data Options = Options { filePath     ∷ String
                       , outputFormat ∷ String
                       }

optParser ∷ Parser Options
optParser = Options
    <$> argument str (metavar "FILE")
    <*> strOption (short 'f'
                   <> long "format"
                   <> metavar "FORMAT")

realMain ∷ Options → IO ()
realMain opts = do m3u ← fromFile $ filePath opts
                   case m3u of
                    Left e → print $ "Error parsing the M3U file: " ++ show e
                    Right m → putStrLn (case (outputFormat opts) of
                                         "markdown" → toOrderedMarkdown m
                                         "yaml" → toYAML m
                                         _ → "Error: invalid outputFormat")

description ∷ String
description = "Convert an M3U playlist to another format"

main ∷ IO ()
main = realMain =<< (execParser $ info (helper <*> optParser)
                                       (fullDesc <> progDesc description
                                        <> header "m3u-converter"))
