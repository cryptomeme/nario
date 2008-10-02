-- 画像ファイルを列挙して、ソースを生成

import System (getArgs)
import FileUtils (fileEntries, basename)
import Text.Regex (mkRegex, matchRegex, matchRegexAll)
import Maybe (isJust)
import Char (toUpper)
import Data.List (intercalate)

-- 画像ファイルの列挙
listupImgFiles path = fileEntries path >>= return . filter bmpFile
	where
		bmpFile = isJust . regex "\\.bmp$"

-- 正規表現
regex rexstr str = matchRegex (mkRegex rexstr) str

-- 全置換
gsub rexstr f str = loop str
	where
		loop str =
			case matchRegexAll rex str of
				Just (before, matched, after, sub)	-> before ++ f sub ++ loop after
				Nothing		-> str
		rex = mkRegex rexstr

-- 拡張子を除いたファイル名
basefn fn =
	case regex "^(.*)\\..*$" $ basename fn of
		Just [base]	-> base
		Nothing		-> fn

-- 先頭を大文字に
camelize (x:xs) = toUpper x : xs

-- エントリ
main = do
	args <- getArgs
	let imgpath = head args

	imgFiles <- listupImgFiles imgpath
	let symbols = map (("Img" ++) . gsub "([a-zA-Z]+)" (camelize . head) . basefn) imgFiles

	putStrLn "module Images (ImageType(..), imageTypes, imageFn) where"
	putStrLn "import Maybe (fromJust)"

	putStrLn $ "data ImageType = " ++ intercalate " | " symbols ++ "\tderiving (Eq)"
	putStrLn $ "imageTypes = [" ++ intercalate ", " symbols ++ "]"
	putStrLn $ "imageFilenames = [\"" ++ intercalate "\", \"" imgFiles ++ "\"]"
	putStrLn $ "imageFn = fromJust . flip lookup (zip imageTypes imageFilenames)"
