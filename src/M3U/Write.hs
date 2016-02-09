{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module M3U.Write ( toM3U
                 , toOrderedMarkdown
                 , toUnorderedMarkdown
                 , toYAML
                 ) where

import           Data.Maybe            (fromJust, isJust)
import           M3U.Common
import           Prelude.Unicode
-- for various output formats
--import           Text.LaTeX.Base.Class    (LaTeXC)
--import           Text.LaTeX.Base.Commands (enumerate, item)
--import           Text.LaTeX.Base.Render   (render)
--import           Text.LaTeX.Base.Syntax   (LaTeX)
--import qualified Text.Latex.Base as LaTeX
-- YAML and required packages
import           Data.ByteString.Char8 (unpack)
import qualified Data.Text             as Text
import qualified Data.Vector           as Vector
import           Data.Yaml             ((.=))
import qualified Data.Yaml             as YAML

toM3U ∷ M3U → String
toM3U = show

-- a general representation of a track's metadata of the format artist - track,
-- e.g. The Beatles - Norwegian Wood
artistHyphenTrack ∷ EXTINF → String
artistHyphenTrack ext = artist ext ++ " - " ++ title ext

-- Does this track have an associated EXTINF?
hasMetadata ∷ M3UEntry → Bool
hasMetadata = isJust ∘ info
-- Get just those tracks that have an associated EXTINF
entriesWithMetadata ∷ M3U → [M3UEntry]
entriesWithMetadata = filter hasMetadata ∘ entries
-- Get only the metadata from those tracks that have it
metadataOnly ∷ M3U → [EXTINF]
metadataOnly = map (fromJust ∘ info) ∘ entriesWithMetadata


-- An ordered markdown list
toOrderedMarkdown ∷ M3U → String
toOrderedMarkdown m3u = snd $ foldr foldFunc (1, "") $ metadataOnly m3u
    where foldFunc entry (i,str) = let prefix = str ++ "\n " ++ show i ++ ". "
                                   in (i+1,prefix ++ artistHyphenTrack entry)
toUnorderedMarkdown ∷ M3U → String
-- An unorderd markdown list
toUnorderedMarkdown m3u = foldr foldFunc "" $ metadataOnly m3u
    where foldFunc entry str = str ++ "\n * " ++ artistHyphenTrack entry

-- convert a single entry to YAML
-- TODO: make M3U, M3UEntry instances of toJSON https://goo.gl/SFn2nl
toYAMLObject ∷ M3UEntry → YAML.Value
toYAMLObject (M3UEntry (Just (EXTINF sec art tit)) path) =
    YAML.object ["title" .= tit, "artist" .= art, "path" .= path]
toYAMLObject (M3UEntry Nothing path) = YAML.object ["path" .= path]

toYAML ∷ M3U → String
toYAML m3u = unpack ∘ YAML.encode ∘ YAML.Array ∘ Vector.fromList $ objs
    where objs = map toYAMLObject (entries m3u)

{-
toLatex ∷ Bool → M3U → String
toLatex ordered m3u = show $ showable
    where showable = render $ enum
          enum = enumerate $ map (item Nothing) ∘ map artistHyphenTrack $ metadataOnly m3u ∷ [LaTeX]
          -}
