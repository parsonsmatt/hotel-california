-- | Like @which(1)@ but portable.
--
-- Modified from @shelly@.
module HotelCalifornia.Which (which) where

import Data.Text qualified as Text
import System.FilePath (splitDirectories, (</>), isAbsolute, searchPathSeparator)
import System.Directory (doesFileExist, getPermissions, getPermissions, executable)
import System.Environment (getEnv)
import Control.Exception (catch)
import Data.Maybe (isJust)

-- | Get a full path to an executable by looking at the @PATH@ environement
-- variable. Windows normally looks in additional places besides the
-- @PATH@: this does not duplicate that behavior.
which :: FilePath -> IO (Maybe FilePath)
which path =
  if isAbsolute path || startsWithDot splitOnDirs
  then checkFile
  else lookupPath
  where
    splitOnDirs = splitDirectories path

    -- 'startsWithDot' receives as input the result of 'splitDirectories',
    -- which will include the dot (\".\") as its first element only if this
    -- is a path of the form \"./foo/bar/baz.sh\". Check for example:
    --
    -- > import System.FilePath as FP
    -- > FP.splitDirectories "./test/data/hello.sh"
    -- [".","test","data","hello.sh"]
    -- > FP.splitDirectories ".hello.sh"
    -- [".hello.sh"]
    -- > FP.splitDirectories ".test/hello.sh"
    -- [".test","hello.sh"]
    -- > FP.splitDirectories ".foo"
    -- [".foo"]
    --
    -- Note that earlier versions of Shelly used
    -- \"system-filepath\" which also has a 'splitDirectories'
    -- function, but it returns \"./\" as its first argument,
    -- so we pattern match on both for backward-compatibility.
    startsWithDot (".":_)  = True
    startsWithDot _ = False

    checkFile :: IO (Maybe FilePath)
    checkFile = do
        exists <- doesFileExist path
        pure $
            if exists
            then Just path
            else Nothing

    lookupPath :: IO (Maybe FilePath)
    lookupPath = (pathDirs >>=) $ findMapM $ \dir -> do
        let fullPath = dir </> path
        isExecutable' <- isExecutable fullPath
        pure $
            if isExecutable'
            then Just fullPath
            else Nothing

    pathDirs =
        (map Text.unpack
          . filter (not . Text.null)
          . Text.split (== searchPathSeparator)
          . Text.pack)
         `fmap` getEnv "PATH"


isExecutable :: FilePath -> IO Bool
isExecutable f = (executable `fmap` getPermissions f) `catch` (\(_ :: IOError) -> return False)


-- | A monadic @findMap@, taken from the @MissingM@ package
findMapM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findMapM _ [] = return Nothing
findMapM f (x:xs) = do
    mb <- f x
    if (isJust mb)
      then return mb
      else findMapM f xs
