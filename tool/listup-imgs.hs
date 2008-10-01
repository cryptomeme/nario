
import System (getArgs)
import FileUtils (fileEntries, basename)
import Text.Regex (mkRegex, matchRegex, matchRegexAll)
import Maybe (isJust)
import Char (toUpper)
import Data.List (intercalate)

-- ‰æ‘œƒtƒ@ƒCƒ‹‚Ì—ñ‹“
listupImgFiles path = fileEntries path >>= return . filter bmpFile
	where
		bmpFile = isJust . regex "\\.bmp$"

regex :: String -> String -> Maybe [String]
regex rex str = matchRegex (mkRegex rex) str

gsub rexstr f str = loop str
	where
		loop str =
			case matchRegexAll rex str of
				Just (before, matched, after, sub)	-> before ++ f sub ++ loop after
				Nothing		-> str
		rex = mkRegex rexstr

basefn fn =
	case regex "^(.*)\\..*$" $ basename fn of
		Just [base]	-> base
		Nothing		-> fn

-- æ“ª‚ğ‘å•¶š‚É
camelize (x:xs) = toUpper x : xs


main = do
	args <- getArgs
	let imgpath = head args

	imgFiles <- listupImgFiles imgpath
	let symbols = map (("Img" ++) . gsub "([a-zA-Z]+)" (camelize . head) . basefn) imgFiles

	putStrLn "module Images (ImageType(..), imageTypes, imageFn) where"
	putStrLn "import Maybe"

	putStrLn $ "data ImageType = " ++ intercalate " | " symbols ++ "\tderiving (Eq)"
	putStrLn $ "imageTypes = [" ++ intercalate ", " symbols ++ "]"
	putStrLn $ "imageFilenames = [\"" ++ intercalate "\", \"" imgFiles ++ "\"]"
	putStrLn $ "imageFn = fromJust . flip lookup (zip imageTypes imageFilenames)"
