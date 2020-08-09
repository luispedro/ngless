{- Copyright 2013-2020 NGLess Authors
 - License: MIT
 -}

module Variable
    ( Variable
    , mkVariable
    , ixVariable
    , varName
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import           Data.IORef (IORef, newIORef, atomicModifyIORef')
import           System.IO.Unsafe (unsafePerformIO)



variableCache :: IORef (M.Map T.Text Int)
variableCache = unsafePerformIO (newIORef M.empty)
{-# NOINLINE variableCache #-}

data Variable = Variable !T.Text !Int
    deriving (Show)

instance Eq Variable where
    (Variable _ a) == (Variable _ b) = a == b

instance Ord Variable where
    (Variable _ a) `compare` (Variable _ b) = a `compare` b

mkVariable :: T.Text -> Variable
mkVariable !n = let !ix = unsafePerformIO $ atomicModifyIORef' variableCache $ \cur ->
                            case M.lookup n cur of
                                Just ix -> (cur, ix)
                                Nothing -> let ix = M.size cur in (M.insert n ix cur, ix)
                in Variable n ix
{-# NOINLINE mkVariable #-}

varName :: Variable -> T.Text
varName (Variable v _) = v

ixVariable :: Variable -> Int
ixVariable (Variable _ ix) = ix
