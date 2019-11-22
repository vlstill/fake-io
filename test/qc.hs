{-# LANGUAGE LambdaCase, TemplateHaskell #-}

-- (c) 2019 Vladimír Štill

import System.Exit ( exitFailure, exitSuccess )
import Prelude hiding (IO, putStr, putStrLn, getLine, readLn, print, readIO, readFile, writeFile, appendFile)
import System.FakeIO
import Test.QuickCheck ( Property, quickCheckAll, (===) )

runEmptyIO = runIO (Input [] mempty)

prop_putStr :: String -> Property
prop_putStr str = runEmptyIO (putStr str) === (Right (), mempty { outputStdout = [str] })

prop_putStrLn :: String -> Property
prop_putStrLn str = runEmptyIO (putStrLn str) === (Right (), mempty { outputStdout = [str ++ "\n"] })

prop_getLine :: String -> Property
prop_getLine str = runIO (Input [str] mempty) getLine === (Right str, mempty)

prop_readLn :: Integer -> Property
prop_readLn val = runIO (Input [show val] mempty) readLn === (Right val, mempty)

prop_print :: Integer -> Property
prop_print val = runEmptyIO (print val) === (Right (), mempty { outputStdout = [show val ++ "\n"]})

prop_readIO :: Integer -> Property
prop_readIO val = runEmptyIO (readIO $ show val) === (Right val, mempty)

prop_catchNothing :: Integer -> Property
prop_catchNothing val  = runEmptyIO (catch (pure val) (error "handler called")) === (Right val, mempty)

prop_throwCatch :: Integer -> Property
prop_throwCatch val = runEmptyIO (catch (throw $ UserError "err") handler) === (Right (val + 42), mempty)
  where
    handler :: IOException -> IO Integer
    handler (UserError "err") = pure (val + 42)
    handler exc = error $ "Unexpected exception " ++ show exc

prop_throw :: Property
prop_throw = runEmptyIO (throw $ UserError "err") ===
             (Left (InterruptException (UserError "err")) :: Either Interrupt (), mempty)

{- TODO
  ,readFile
  ,writeFile
  ,appendFile
  ,doesFileExist
  ,removeFile
  ,getDirectoryContents
-}

pure []

main = $(quickCheckAll) >>= \case
          True  -> exitSuccess
          False -> exitFailure
