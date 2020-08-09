{- Copyright 2013-2020 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts #-}

module Validation
    ( validate
    , uses_STDOUT
    ) where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Either.Combinators (rightToMaybe)
import           Control.Monad.Extra (whenJust)
import           Control.Monad.Writer.Strict
import           Control.Monad.RWS
import           Control.Monad (foldM_)
import           Data.List (find, isSuffixOf)
import           Data.Maybe
import           Data.Char (isUpper)
import           Data.Foldable (asum)

import Language
import Modules
import NGLess.NGError
import BuiltinFunctions
import Utils.Suggestion


findMethod :: MethodName -> Maybe MethodInfo
findMethod m = find ((==m) . methodName) builtinMethods

-- | Returns either an error message if it finds any errors or the input script unscathed
validate :: [Module] -> Script -> NGLess Script
validate mods expr = case errors of
        [] -> Right expr
        _ -> throwScriptError . concat . addNL . map T.unpack $ errors
    where
        addNL [] = []
        addNL [e] = [e]
        addNL (e:es) = e:"\n":addNL es
        errors = concatMap (\f -> execWriter (f mods expr)) checks
        checks :: [[Module] -> Script -> Writer [T.Text] ()]
        checks =
            [validateVariables
            ,validateFunctionReqArgs -- check for the existence of required arguments in functions.
            ,validateSymbolInArgs
            ,validateSTDINusedOnce
            ,validateMapRef
            ,validateNoConstantAssignments
            ,validateNGLessVersionUses
            ,validatePureFunctions
            ,validateWriteOName
            ,validateBlockAssignments
            ]

{- Each checking function has the type
 -
 - Script -> Maybe T.Text
 -
 - If it finds an error, it returns a Just error; otherwise, Nothing.
 -
 - The validate function just runs all checks and either concatenates all the
 - error messages or passes the script unharmed on the Right side.
 -}


-- | check whether results of calling pure functions are use
validatePureFunctions mods (Script h es) =
        forM_ es $ \(lno, expr) -> case expr of
            FunctionCall (FuncName "preprocess") _ _ _
                | not (version00 h) -> tell1lno lno ["Preprocess must be assigned to an output (behaviour changed from version 0.0)"]
                | otherwise -> return ()
            FunctionCall fname@(FuncName f) _ _ _
                | isPure fname -> tell1lno lno ["Result of calling function `",  f, "` should be assigned to a variable (this function has no effect otherwise)."]
            _ -> return ()

    where
        isPure f = FunctionCheckReturnAssigned `elem` (fromMaybe [] $ funcChecks <$> findFunction mods f)
        version00 (Just (Header "0.0" _)) = True
        version00 _ = False

validateFunctionReqArgs :: [Module] -> Script -> Writer [T.Text] ()
validateFunctionReqArgs mods = checkRecursiveScript validateFunctionReqArgs'
    where
        validateFunctionReqArgs' (FunctionCall f _ args _) = case findFunction mods f of
                Nothing -> Just (T.concat ["Function ", T.pack . show $ f, " not found."])
                Just finfo -> errors_from_list $ map has1 (funcKwArgs finfo)
                    where
                        used = map (\(k, _) -> varName k) args
                        has1 ainfo = if not (argRequired ainfo) || argName ainfo `elem` used
                            then Nothing
                            else Just (T.concat ["Function ", T.pack . show $ f, " requires argument ", argName ainfo, "."])
        validateFunctionReqArgs' _ = Nothing

validateVariables :: [Module] -> Script -> Writer [T.Text] ()
validateVariables mods (Script _ es) = runChecker $ forM_ es $ \(_,e) -> case e of
        Assignment v e' -> do
            vs <- get
            recursiveAnalyse checkVarUsage e'
            put (v:vs)
        _ -> recursiveAnalyse checkVarUsage e
    where

        runChecker :: RWS () [T.Text] [Variable] () -> Writer [T.Text] ()
        runChecker c = tell . snd . evalRWS c () $ (mkVariable . fst <$> concatMap modConstants mods)

        checkVarUsage :: Expression -> RWS () [T.Text] [Variable] ()
        checkVarUsage (Lookup _ v) = do
                used <- get
                when (v `notElem` used) $
                    tell [T.concat ["Could not find variable `", varName v, "`. ", suggestionMessage (varName v) (map varName used)]]
        checkVarUsage (FunctionCall _ _ _ (Just block)) = do
            vs <- get
            let v' = blockVariable block
            put (v':vs)
        checkVarUsage (Assignment v _) = do
            vs <- get
            put (v:vs)
        checkVarUsage _ = return ()

validateSymbolInArgs :: [Module] -> Script -> Writer [T.Text] ()
validateSymbolInArgs mods = checkRecursiveScriptWriter validateSymbolInArgs'
    where
        validateSymbolInArgs' (FunctionCall f _ args _) = checkFunction f args
        validateSymbolInArgs' (MethodCall m _ arg0 args) = checkMethod m arg0 args
        validateSymbolInArgs' _ = return ()

        checkFunction :: FuncName -> [(Variable, Expression)]-> Writer [T.Text] ()
        checkFunction f args = case findFunction mods f of
                Nothing -> tell [T.concat ["Function '", T.pack . show $ f, "' not found"]]
                Just finfo -> mapM_ (check1 finfo) args
            where
                check1 :: Function -> (Variable, Expression) -> Writer [T.Text] ()
                check1 finfo (v, expr) = let legal = allowedFunction finfo (varName v) in case expr of
                        ConstSymbol s
                            | s `notElem` legal -> tell . (:[]) . T.concat $
                                case findSuggestion s legal of
                                    Nothing ->
                                        ["Argument: `", varName v, "` (for function ", T.pack (show f), ") expects one of ", showA legal, " but got {", s, "}"]
                                    Just (Suggestion valid reason) ->
                                        ["Argument `", varName v, "` for function ", T.pack (show f), ", got {", s, "}.\n\tDid you mean {", valid, "} (", reason, ")\n\n",
                                        "Legal arguments are: [", showA legal, "]\n"]
                        ListExpression es   -> mapM_ (\e -> check1 finfo (v, e)) es
                        _                   -> return ()

        allowedFunction :: Function -> T.Text -> [T.Text]
        allowedFunction finfo v = fromMaybe [] $ do
            argInfo <- find ((==v) . argName) (funcKwArgs finfo)
            ArgCheckSymbol ss <- find (\case { ArgCheckSymbol{} -> True; _ -> False }) (argChecks argInfo)
            return ss


        allowedMethod minfo v = fromMaybe [] $ do
            argInfo <- find ((==v) . argName) (methodKwargsInfo minfo)
            ArgCheckSymbol ss <- find (\case { ArgCheckSymbol{} -> True; _ -> False}) (argChecks argInfo)
            return ss

        checkMethod m (Just a) args = checkMethod m Nothing ((mkVariable "__0", a):args)
        checkMethod m Nothing args = case findMethod m of
                Nothing -> tell [T.concat ["Method'", T.pack . show $ m, "' not found"]]
                Just minfo -> mapM_ (check1m minfo) args
            where
                check1m minfo (v, expr) = let legal = allowedMethod minfo (varName v) in case expr of
                    ConstSymbol s
                        | s `notElem` legal ->  tell . (:[]) . T.concat $
                            case findSuggestion s legal of
                                Nothing ->
                                    (if v /= mkVariable "__0" then ["Argument `", varName v, "` "] else ["Unnamed argument "]) ++ ["(for method ", unwrapMethodName m, ") expects one of ", showA legal, " but got {", s, "}"]
                                Just (Suggestion valid reason) ->
                                    (if v /= mkVariable "__0" then ["Argument `", varName v, "` "] else ["Unnamed argument "]) ++ ["(for method ", unwrapMethodName m, ") got {", s, "}"] ++
                                        ["\n\tDid you mean {", valid, "} (", reason, ")\n\nAllowed arguments are: [", showA legal, "]"]
                    ListExpression es   -> mapM_ (\e -> check1m minfo (v, e)) es
                    _                   -> return ()

        showA [] = ""
        showA [e] = T.concat ["{", e, "}"]
        showA (e:es) = T.concat ["{", e, "}, ", showA es]



validateMapRef :: [Module] -> Script -> Writer [T.Text] ()
validateMapRef _ = checkRecursiveScript validateMapRef'
    where
        validateMapRef' (FunctionCall (FuncName "map") _ args _) =
            case (lookup (mkVariable "reference") args, lookup (mkVariable "fafile") args) of
                (Nothing, Nothing) -> Just "Either fafile or reference must be specified in argument to map function"
                (Just _, Just _) -> Just "You cannot specify both fafile and reference in arguments to map function"
                _ -> Nothing
        validateMapRef' _ = Nothing

validateWriteOName :: [Module] -> Script -> Writer [T.Text] ()
validateWriteOName _ = checkRecursiveScript $ validateWriteOName'
    where
        validateWriteOName' (FunctionCall (FuncName "write") (Lookup (Just t) _) args _) =
            lookup (mkVariable "oname") args >>= staticValue >>= \case
                NGOString oname -> case lookup (mkVariable "format") args of
                    Nothing -> checkType t (T.unpack oname)
                    Just _ -> Nothing
                _ -> Nothing
        validateWriteOName' _ = Nothing
        checkType NGLReadSet oname
            | isSuffixOf ".fa" oname = Just "Cannot save data in FASTA format."
            | isSuffixOf ".fq" oname = Nothing
            | isSuffixOf ".fq.gz" oname = Nothing
            | otherwise = Just . T.concat $ ["Cannot determine output format from filename '", T.pack oname, "'"]
        checkType _ _ = Nothing


validateSTDINusedOnce :: [Module] -> Script -> Writer [T.Text] ()
validateSTDINusedOnce _ (Script _ code) = foldM_ validateSTDINusedOnce' Nothing code
    where
        validateSTDINusedOnce' :: Maybe Int -> (Int, Expression) -> Writer [T.Text] (Maybe Int)
        validateSTDINusedOnce' s (lno,e)
            | constant_used "STDIN" e = do
                whenJust s $ \prev ->
                    tell1lno lno ["STDIN can only be used once (previously used on line ", T.pack (show prev), ")."]
                return $ Just lno
            | otherwise = return s


constant_used :: T.Text -> Expression -> Bool
constant_used k (BuiltinConstant k') = k == (varName k')
constant_used k (ListExpression es) = constant_used k `any` es
constant_used k (UnaryOp _ e) = constant_used k e
constant_used k (BinaryOp _ a b) = constant_used k a || constant_used k b
constant_used k (Condition a b c) = constant_used k a || constant_used k b || constant_used k c
constant_used k (IndexExpression a ix) = constant_used k a || constant_used_ix k ix
constant_used k (Assignment _ e) = constant_used k e
constant_used k (FunctionCall _ e args b) = constant_used k e || constant_used k `any` [e' | (_,e') <- args] || constant_used_block k b
constant_used k (Sequence es) = constant_used k `any` es
constant_used _ _ = False
constant_used_ix k (IndexOne a) = constant_used k a
constant_used_ix k (IndexTwo a b) = constant_used_maybe k a || constant_used_maybe k b
constant_used_maybe k (Just e) = constant_used k e
constant_used_maybe _ Nothing = False
constant_used_block k (Just (Block _ e)) = constant_used k e
constant_used_block _ _ = False

uses_STDOUT :: Expression -> Bool
uses_STDOUT = constant_used "STDOUT"

validateNoConstantAssignments :: [Module] -> Script -> Writer [T.Text] ()
validateNoConstantAssignments mods (Script _ es) = foldM_ checkAssign builtins es
    where
        checkAssign :: [T.Text] -> (Int,Expression) -> Writer [T.Text] [T.Text]
        checkAssign active (lno,e) = case e of
            Assignment v _ -> do
                when (varName v `elem` active) $
                    tell1lno lno ["assignment to constant `", varName v, "` is illegal."]
                return $ if T.all isUpper (varName v)
                            then varName v:active
                            else active
            _ -> return active
        builtins = ["STDIN", "STDOUT"] ++ (fst <$> concatMap modConstants mods)


addLno lno errs = [T.concat ["Error on line ", T.pack (show lno), ": ", e] | e <- errs]

checkRecursiveScriptWriter :: (Expression -> Writer [T.Text] ()) -> Script -> Writer [T.Text] ()
checkRecursiveScriptWriter f (Script _ es) = forM_ es $ \(lno, e) ->
        censor (addLno lno) $ recursiveAnalyse f e

checkRecursiveScript :: (Expression -> Maybe T.Text) -> Script -> Writer [T.Text] ()
checkRecursiveScript f (Script _ es) = forM_ es $ \(lno, e) ->
        censor (addLno lno) $ recursiveAnalyse f' e
    where
        f' :: Expression -> Writer [T.Text] ()
        f' e' = whenJust (f e') (tell . (:[]))

errors_from_list :: [Maybe T.Text] -> Maybe T.Text
errors_from_list errs = case catMaybes errs of
    [] -> Nothing
    errs' -> Just (T.concat errs')

tell1lno :: Int -> [T.Text] -> Writer [T.Text] ()
tell1lno lno err = tell [T.concat $ ["Line ", T.pack (show lno), ": "] ++ err]

validateNGLessVersionUses :: [Module] -> Script -> Writer [T.Text] ()
validateNGLessVersionUses mods sc = case nglVersion <$> nglHeader sc of
        Nothing -> return ()
        Just version -> forM_ (nglBody sc) $ \(lno, expr) ->
            recursiveAnalyse (check version lno) expr
    where
        check :: T.Text -> Int -> Expression -> Writer [T.Text] ()
        check version lno f = case f of
                FunctionCall fname@(FuncName fname') _ kwargs _ ->
                    whenJust (findFunction mods fname) $ \finfo -> do
                        checkVersion ["Function ", fname'] $ minVersionFunction finfo
                        checkVersionChanged ["Function ", fname'] $ minVersionFunctionChanged finfo
                        forM_ kwargs $ \(name,_) ->
                            checkVersion ["Using argument ", varName name, " to function ", fname'] $ checkArg (funcKwArgs finfo) (varName name)
                MethodCall mname@(MethodName mname') _ _ kwargs ->
                    whenJust (findMethod mname) $ \minfo -> do
                        checkVersion ["Using method ", mname'] $ minVersionMethod minfo
                        forM_ kwargs $ \(name, _) ->
                            checkVersion ["Using argument ", varName name, " to method ", mname'] $ checkArg (methodKwargsInfo minfo) (varName name)
                _ -> return ()
            where
                showV (a,b) = T.pack (show a ++ "." ++ show b)
                checkVersion _ Nothing = return ()
                checkVersion prefix (Just minV)
                    | versionLE minV version = return ()
                    | otherwise = tell1lno lno (prefix ++ [" requires ngless version ", showV minV, " (version '", version, "' is active)."])
                checkVersionChanged _ Nothing = return ()
                checkVersionChanged prefix (Just minV)
                    | versionLE minV version = return ()
                    | otherwise = tell1lno lno (prefix ++ [" changed behaviour in an incompatible fashion in version ", showV minV, " (version '", version, "' is active).\n",
                                                           "See https://ngless.embl.de/whatsnew.html for details on changes."])
        minVersionFunction :: Function -> Maybe (Int, Int)
        minVersionFunction finfo =
            asum $ flip map (funcChecks finfo) $ \case
                            FunctionCheckMinNGLessVersion minV -> Just minV
                            _ -> Nothing

        minVersionFunctionChanged :: Function -> Maybe (Int, Int)
        minVersionFunctionChanged finfo =
            asum $ flip map (funcChecks finfo) $ \case
                            FunctionCheckNGLVersionIncompatibleChange minV -> Just minV
                            _ -> Nothing

        minVersionMethod :: MethodInfo -> Maybe (Int, Int)
        minVersionMethod minfo =
            asum $ flip map (methodChecks minfo) $ \case
                            FunctionCheckMinNGLessVersion minV -> Just minV
                            _ -> Nothing

        checkArg :: [ArgInformation] -> T.Text -> Maybe (Int, Int)
        checkArg ainfos argname = do
            ainfo <- find ((== argname) . argName) ainfos
            minVersion (argChecks ainfo)

        minVersion [] = Nothing
        minVersion (ArgCheckMinVersion minV:_) = Just minV
        minVersion (_:rs) = minVersion rs
        versionLE (majV, minV) actual = case parseVersion actual of
            Just (aMaj, aMin) -> case aMaj `compare` majV of
                GT -> True
                EQ -> aMin >= minV
                LT -> False
            _ -> False
        parseVersion :: T.Text -> Maybe (Int, Int)
        parseVersion version = do
            (majV, rest) <- rightToMaybe $ T.decimal version
            guard $ not (T.null rest)
            (minV, _) <- rightToMaybe $ T.decimal (T.tail rest)
            return (majV, minV)


-- Check that only block variables are assigned inside a block
validateBlockAssignments :: [Module] -> Script -> Writer [T.Text] ()
validateBlockAssignments _ (Script _ es) = forM_ es validateBlockAssignments1
validateBlockAssignments1 :: (Int, Expression) -> Writer [T.Text] ()
validateBlockAssignments1 (lno, e) = case e of
    Assignment _ e' -> validateBlockAssignments1 (lno, e')
    FunctionCall (FuncName fname)  _ _ (Just block) -> let var = blockVariable block
                                        in recursiveAnalyse (checkAssignmentOnlyTo fname lno var) (blockBody block)
    _ -> return ()
checkAssignmentOnlyTo fname lno v e = case e of
    Assignment v' _
        | v /= v' -> tell1lno lno ["Inside blocks, only the block variable (in this case `", varName v, "`) can be assigned to",
                                    " (when analysing function `", fname, "`)."]
    _ -> return ()
