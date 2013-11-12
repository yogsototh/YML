import              Control.Monad           (foldM)
import              System.Console.GetOpt
import              System.Environment     (getArgs,getProgName)
import              System.IO              (readFile)

import              YML.Dataset
import              YML.LinearGradient

-- | Our main function takes a file as parameter
-- and try to read it and put its values inside a dataset data structure
main :: IO ()
main = do
    (options,file)  <- parseArgs
    fileContent     <- readFile file
    let trainingSet = parse fileContent
    -- print trainingSet
    putStr "Cost of training set using the null function: "
    print $ cost trainingSet (nullF trainingSet)
    putStr "Function minimizing the cost: "
    print $ gradientDescent (optionsToParameters options) trainingSet
    where
        optionsToParameters :: Options -> Parameters
        optionsToParameters (Options valAlpha valThreshold) = Parameters valAlpha valThreshold

data Options   = Options { optAlpha :: R , optThreshold :: R} deriving Show
defaultOptions = Options { optAlpha = 0.01, optThreshold = 10**(-10) }

parseArgs :: IO (Options,String)
parseArgs = do
    argv <- getArgs
    progName <- getProgName
    let header = "Usage: " ++ progName ++ " [OPTIONS...] file"
    let helpMessage = usageInfo header options
    case getOpt RequireOrder options argv of
        (opts, [file], []) ->
            case foldM (flip id) defaultOptions opts of
                Right opts -> return (opts,file)
                Left errorMessage -> ioError (userError (errorMessage ++ "\n" ++ helpMessage))
        (_,_,errs) -> ioError (userError (concat errs ++ helpMessage))

options :: [OptDescr (Options -> Either String Options)]
options = [
      Option ['a'] ["alpha"] (ReqArg (\a opts -> case reads a of
                                        [(a, "")] | a>0 -> Right opts { optAlpha = a }
                                        _ -> Left "--alpha must be >0"
                                      ) "alpha") "set alpha value"
    , Option ['t'] ["threshold"]  (ReqArg (\a opts -> case reads a of
                                        [(t, "")] | t>0 -> Right opts { optThreshold = t }
                                        _ -> Left "--threshold must be >0"
                                      ) "threshold") "set threshold value"
    ]

