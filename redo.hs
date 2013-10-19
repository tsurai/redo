import Control.Monad
import Data.Maybe
import System.Directory
import System.Process
import System.Exit
import System.FilePath
import System.IO
import System.Environment

main :: IO ()
main = mapM_ redo =<< getArgs

redo :: String -> IO ()
redo target = maybe printMissing redo' =<< redoPath target
  where redo' :: FilePath -> IO ()
        redo' path = do
          (_, _, _, ph) <- createProcess $ shell $ cmd path
          exit <- waitForProcess ph
          case exit of
            ExitSuccess -> do renameFile tmp target
            ExitFailure code -> do hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
                                   removeFile tmp
        tmp = target ++ "---redoing"
        printMissing = error $ "No .do file found for target '" ++ target ++ "'" 
        cmd path = unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe `liftM` filterM doesFileExist candidates 
  where candidates = [target ++ ".do"] ++ if hasExtension target 
                                          then [replaceBaseName target "default" ++ ".do"]
                                          else []
