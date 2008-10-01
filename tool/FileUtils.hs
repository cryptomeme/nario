
{-
http://www.loveruby.net/ja/stdhaskell/samples/lazylines/FileUtils.hs.html
http://www.loveruby.net/ja/stdhaskell/samples/lazylines/PathUtils.hs.html
を参考に
-}

module FileUtils where

import System.Directory
import Control.Monad
import Data.List

-- #ifdef POSIX
pathSep = '/'
pathSepStr = "/"
-- #elif WIN32
-- pathSep = '\\'
-- pathSepStr = "\\"
-- #endif

-- ディレクトリ内の項目（ファイル、ディレクトリ）を列挙
dirEntries :: FilePath -> IO [String]
dirEntries path = return . filter notDotFile =<< getDirectoryContents path
	where
		notDotFile fn = (head fn) /= '.'

-- ディレクトリ内のファイルを列挙
fileEntries :: FilePath -> IO [String]
fileEntries path = filterM isFile =<< dirEntries path
	where
		isFile name = doesFileExist (joinPath path name)

-- パスの結合
joinPath a b = a ++ "/" ++ b





rootPath = pathSepStr

isRoot :: String -> Bool
isRoot = (== rootPath)

dirname :: String -> String
dirname = fst . splitPath

basename :: String -> String
basename = snd . splitPath

splitPath :: String -> (String, String)
splitPath ""   = error "splitPath \"\""
splitPath path = case splitPath' path of
                   ("", (_:[])) -> (pathSepStr, pathSepStr)
                   (b,  ""    ) -> (".",        reverse b)
                   (b,  (_:[])) -> (pathSepStr, reverse b)
                   (b,  (_:d) ) -> (reverse d,  reverse b)
  where
    splitPath' = break (== pathSep) . dropSep . uniqSeps . reverse

    dropSep path | isRoot path = rootPath
                 | otherwise   = dropWhile (== pathSep) path

    uniqSeps ""   = ""
    uniqSeps path = let ps = group path
                    in if head (head ps) == pathSep
                       then pathSep : concat (tail ps)
                       else path
