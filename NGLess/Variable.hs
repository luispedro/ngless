{- Copyright 2013-2020 NGLess Authors
 - License: MIT
 -}

module Variable
    ( Variable
    , mkVariable
    , varName
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import           System.IO.Unsafe (unsafePerformIO)
import           Data.IORef (IORef, newIORef, modifyIORef, readIORef)


type CacheT = M.Map T.Text Int

variableCache :: IORef CacheT
variableCache = unsafePerformIO (newIORef M.empty)
{-# NOINLINE variableCache #-}

data Variable = Variable !T.Text !Int
    deriving (Show)

instance Eq Variable where
    (Variable _ a) == (Variable _ b) = a == b

instance Ord Variable where
    (Variable _ a) `compare` (Variable _ b) = a `compare` b

mkVariable :: T.Text -> Variable
mkVariable n = unsafePerformIO $ do
    cur <- readIORef variableCache
    case M.lookup n cur of
        Just ix -> return $! Variable n ix
        Nothing -> do
            let ix = M.size cur
            modifyIORef variableCache (M.insert n ix)
            return $! Variable n ix
varName (Variable v _) = v

