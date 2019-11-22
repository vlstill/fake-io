{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Pure IO monad, intended for educational use.

module System.FakeIO
  (-- * The IO monad and its machinery
    runIO
  , IO
  , Input(..)
  , Output(..)
  , Interrupt(..)
  -- * Library of actions
  , IOException(..)
  , putStrLn
  , putStr
  , getLine
  , readLn
  , print
  , readIO
  , throw
  , catch
  , readFile
  , writeFile
  , appendFile
  , doesFileExist
  , removeFile
  , getDirectoryContents
  )
  where

import           Control.Applicative ( liftA2 )
import           Control.Arrow ( first, second )
import           Control.Monad.Except ( ExceptT, runExceptT, throwError, catchError )
import           Control.Monad.State ( State, runState, modify, get, gets )
import           Data.Map ( Map )
import qualified Data.Map as M
import           Data.Maybe ( fromMaybe, isJust )
import           Data.Monoid ( Monoid ( mappend, mempty ) )
import           Data.Semigroup ( Semigroup ( (<>) ) )
import           Prelude hiding ( IO, putStr, putStrLn, getLine, readLn, print, readIO, readFile, writeFile, appendFile )
import           Data.List ( isPrefixOf, isSuffixOf )
import           Text.Read ( readMaybe )

--------------------------------------------------------------------------------
-- IO monad and machinery

-- | An IO exception.
data IOException = UserError String
                 | FileNotFound FilePath
                 | DirectoryNotFound FilePath
  deriving (Show, Read, Eq)

-- | User input.
data Input = Input
  { inputStdin :: ![String]
  , inputFiles :: !(Map String String)
  } deriving (Show, Eq)

instance Semigroup Input where
  Input lns0 files0 <> Input lns1 files1 = Input (lns0 <> lns1) (files0 <> files1)

instance Monoid Input where
  mempty = Input mempty mempty

-- | IO monad output.
data Output = Output
  { outputStdout :: ![String]
  , outputFiles  :: !(Map String String)
  } deriving (Show, Read, Eq)

instance Semigroup Output where
  Output a x <> Output b y = Output (a <> b) (x <> y)

instance Monoid Output where
  mempty = Output mempty mempty
  mappend = (<>)

-- | Something that interrupts the flow of the IO monad.
data Interrupt
  = InterruptStdin -- ^ When you receive this interrupt, you should
                   -- get some standard input from somewhere and then
                   -- provide it in the 'Input' value next time you
                   -- call 'runIO'.
  | InterruptException !IOException -- ^ When you receive this
                                    -- interrupt, you should consider
                                    -- the computation as ended.
  deriving (Show, Read, Eq)

-- | A pure IO monad.
newtype IO a = IO
  { unIO :: ExceptT Interrupt (State (Input,Output)) a
  }
  -- We purposely don't derive MonadState and MonadError, while it
  -- would aid programming minutely, such instances are internals that
  -- we don't want to export.
  deriving (Monad, Functor, Applicative)

instance Semigroup a => Semigroup (IO a) where
  (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (IO a) where
  mempty = pure mempty
  mappend = (<>)

-- | Run the IO monad. This should be called in succession. Depending
-- on the type of interrupt, this function should be re-run with the
-- same action but with additional input.
runIO :: Input -> IO a -> (Either Interrupt a, Output)
runIO input m =
  second snd
         (runState (runExceptT (unIO m))
                   (input { inputFiles = mempty }
                   ,mempty { outputFiles = inputFiles input }))

-- | Interrupt the IO monad. This stops the IO monad computation,
-- allowing for any resumption later.
interrupt :: Interrupt -> IO a
interrupt = IO . throwError

-- | Modify the given file.
modifyFile :: FilePath -> (String -> String) -> IO ()
modifyFile fp f = modifyFiles (M.alter (Just . f . fromMaybe "") fp)

-- | Modify the output files.
modifyFiles :: (Map FilePath String -> Map FilePath String) -> IO ()
modifyFiles f = IO (modify (\(i,o) -> (i,updateFile o)))
  where updateFile (Output stdout files) = Output stdout (f files)

--------------------------------------------------------------------------------
-- Library

-- | The same as 'putStr', but adds a newline character.
putStrLn :: String -> IO ()
putStrLn = putStr . (++ "\n")

-- | Write a string to the standard output device.
putStr :: String -> IO ()
putStr new = IO (modify (\(i,o) -> (i,o <> Output [new] mempty)))

-- | Read a line from standard input.
getLine :: IO String
getLine = do
  (Input is fs,_) <- IO get
  case is of
    [] -> interrupt InterruptStdin
    (i:is') -> do IO (modify (first (const (Input is' fs))))
                  return i

-- | The 'readIO' function is similar to 'read' except that it signals
-- parse failure to the 'IO' monad instead of terminating the program.
readIO :: Read a => String -> IO a
readIO s =
  case readMaybe s of
    Nothing -> throw (UserError "readIO: no parse")
    Just r -> return r

-- | The readLn function combines 'getLine' and 'readIO'.
readLn :: Read a => IO a
readLn = getLine >>= readIO

-- | The 'print' function outputs a value of any printable type to the
-- standard output device.
-- Printable types are those that are instances of class 'Show'; 'print'
-- converts values to strings for output using the 'show' operation and
-- adds a newline.
--
-- For example, a program to print the first 20 integers and their
-- powers of 2 could be written as:
--
-- > main = print ([(n, 2^n) | n <- [0..19]])
print :: Show a => a -> IO ()
print = putStrLn . show

-- | Throw an IO exception.
throw :: IOException -> IO a
throw = interrupt . InterruptException

-- | Catch an IO exception.
catch :: IO a -> (IOException -> IO a) -> IO a
catch (IO m) f = IO (catchError m handler)
  where handler i =
          case i of
            InterruptException e ->
              let (IO m') = f e
              in m'
            _ -> throwError i


-- | The 'readFile' function reads a file and
-- returns the contents of the file as a string.
-- The file is read lazily, on demand, as with 'getContents'.
readFile :: FilePath -> IO String
readFile fp =
  do mbytes <- IO (gets (M.lookup fp . outputFiles . snd))
     case mbytes of
       Nothing -> throw (FileNotFound fp)
       Just bytes -> return bytes

-- | The computation 'writeFile' @file str@ function writes the string @str@,
-- to the file @file@.
writeFile :: FilePath -> String -> IO ()
writeFile fp = modifyFile fp . const

-- | The computation 'appendFile' @file str@ function appends the string @str@,
-- to the file @file@.
--
-- Note that 'writeFile' and 'appendFile' write a literal string
-- to a file.  To write a value of any printable type, as with 'print',
-- use the 'show' function to convert the value to a string first.
--
-- > main = appendFile "squares" (show [(x,x*x) | x <- [0,0.1..2]])
appendFile :: FilePath -> String -> IO ()
appendFile fp more = modifyFile fp (++ more)

-- | The operation 'doesFileExist' returns 'True' if the argument file
-- exists, and 'False' otherwise.
doesFileExist :: FilePath -> IO Bool
doesFileExist fp = fmap isJust (IO (gets (M.lookup fp . outputFiles . snd)))

-- | 'removeFile' /file/ removes the directory entry for an existing
-- file /file/.
removeFile :: FilePath -> IO ()
removeFile fp = do
  exists <- doesFileExist fp
  if exists
     then modifyFiles (M.delete fp)
     else throw (FileNotFound fp)

-- | Get all files in the given directory.
getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents fp =
  do entries <- IO (gets (M.keys . outputFiles . snd))
     case filter (isPrefixOf fp') entries of
       [] -> throw (DirectoryNotFound fp)
       fs -> return fs
  where fp' | "/" `isSuffixOf` fp = fp
            | otherwise = fp ++ "/"
