{-# LANGUAGE Safe          #-}
{-# LANGUAGE UnicodeSyntax #-}
module M3U.Common ( M3U(..)
                  , M3UEntry(..)
                  , EXTINF(..)
                  ) where
import           Prelude.Unicode
import           System.FilePath.Posix (FilePath)

-- |The 'M3U' datatypes describe various subcomponents of the M3U format.
-- `show`ing a whole M3U should result in it being converted precisely back into
-- a valid .m3u representation.
data M3U = M3U { entries ∷ [M3UEntry] } deriving (Eq)
instance Show M3U where
    show m3u = foldr (\v acc → acc ++ v) "#EXTM3U\n" (map show $ entries m3u)

data M3UEntry = M3UEntry { info ∷ Maybe EXTINF, path ∷ FilePath } deriving (Eq)
instance Show M3UEntry where
    show entry = (case (info entry) of Just extinfo → show extinfo ++ "\n"
                                       Nothing → "") ++ (path entry) ++ "\n"

data EXTINF = EXTINF { seconds ∷ Int , artist ∷ String , title ∷ String }
    deriving (Eq)
instance Show EXTINF where
    show ext = "#EXTINF:" ++ show (seconds ext) ++ artist ext ++ title ext
