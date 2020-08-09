{- Copyright 2017-2020 NGLess Authors
 - License: MIT
 -}

module JSONScript
    ( writeScriptJSON
    ) where

import           System.IO.SafeWrite (withOutputFile)
import           Data.Aeson
import           Data.Aeson.Types (Pair)
import           Control.Arrow (second)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import Language


newtype EncHeader = EncHeader Header
instance ToJSON EncHeader where
    toJSON (EncHeader (Header ver mods)) = object
                            [ "ngless-version" .= toJSON ver
                            , "modules" .= toJSON (encodeMod <$> mods)
                            ]

jsonType :: String -> Pair
jsonType = ("type" .=)

sP :: T.Text -> T.Text -> Pair
sP a b = a .= b


encodeMod (ModInfo n v) = object [jsonType "module", "name" .= n, "version" .= v]
encodeMod (LocalModInfo n v) = object [jsonType "local-module", "name" .= n, "version" .= v]
newtype EncExpression = EncExpression Expression

instance ToJSON EncExpression where
    toJSON (EncExpression e) = toJSONEx e

toJSONEx :: Expression -> Value
toJSONEx (Lookup t n) = object [jsonType "lookup", "name" .= toJSON (varName n), "ngless-type" .= encodeMaybeType t]
toJSONEx (ConstStr t) = toJSON t
toJSONEx (ConstInt i) = toJSON i
toJSONEx (ConstDouble d) = toJSON d
toJSONEx (ConstBool b) = toJSON b
toJSONEx (ConstSymbol s) = toJSON (T.concat ["{", s, "}"])
toJSONEx (BuiltinConstant v) = toJSON (varName v)
toJSONEx (ListExpression exprs) = toJSON (toJSONEx <$> exprs)
toJSONEx Continue = object [jsonType "control0", "op" `sP` "continue"]
toJSONEx Discard = object [jsonType "control0", "op" `sP` "discard"]
toJSONEx (UnaryOp uop e) = object [jsonType "uop", "op" .= encodeUOp uop, "arg" .= toJSONEx e]
toJSONEx (BinaryOp bop el er) = object [jsonType "binop", "op" .= encodeBOp bop, "left" .= toJSONEx el, "right" .= toJSONEx er]
toJSONEx (Condition ec eT eF) = object [jsonType "control", "op" `sP` "if", "cond" .= toJSONEx ec, "if-true" .= toJSONEx eT, "if-false" .= toJSONEx eF]
toJSONEx (IndexExpression e ix) = object [jsonType "index", "arg" .= toJSONEx e, "index" .= toJSONIndex ix]
toJSONEx (Assignment n e) = object [jsonType "assignment", "target" .= toJSON (varName n), "value" .= toJSONEx e]
toJSONEx (FunctionCall (FuncName fn) e kwargs block) = object [jsonType "function", "fname" .= toJSON fn, "arg0" .= toJSONEx e, "kwargs" .= toJSONKwArgs kwargs, "block" .= encodeBlock block]
toJSONEx (MethodCall (MethodName mn) ethis marg kwargs) = object [jsonType "method", "mname" .= toJSON mn, "this" .= toJSONEx ethis, "arg0" .= toJSON (EncExpression <$> marg), "kwargs" .= toJSONKwArgs kwargs]
toJSONEx (Sequence es) = object [jsonType "control", "op" `sP` "sequence", "args" .= toJSON (toJSONEx <$> es)]
toJSONEx (Optimized oe) = object [jsonType "optimized", "value" .= encodeOpt oe]

toJSONIndex (IndexOne e) = object [jsonType "index1", "arg" .= toJSONEx e]
toJSONIndex (IndexTwo e0 e1) = object [jsonType "index2", "left" .= maybe Null toJSONEx e0, "right" .= maybe Null toJSONEx e1]

toJSONKwArgs kwargs = object [varName n .= toJSONEx e | (n, e) <- kwargs]

encodeOpt (LenThresholdDiscard n bop t) = object [jsonType "len-threshold", "name" .= toJSON (varName n), "op" .= encodeBOp bop, "thresh" .= toJSON t]
encodeOpt (SubstrimReassign n mq) = object [jsonType "substrim-reassign", "name" .= toJSON (varName n), "minqual" .= toJSON mq]

encodeBlock Nothing = Null
encodeBlock (Just (Block var e)) = object [jsonType "block", "variables" .= [toJSON (varName var)], "body" .= toJSONEx e]

encodeBOp :: BOp -> T.Text
encodeBOp BOpAdd = "add"
encodeBOp BOpMul = "mul"
encodeBOp BOpGT = "gt"
encodeBOp BOpGTE = "gte"
encodeBOp BOpLT = "lt"
encodeBOp BOpLTE = "lte"
encodeBOp BOpEQ = "eq"
encodeBOp BOpNEQ = "neq"

encodeBOp BOpPathAppend = "path_append"

encodeUOp :: UOp -> String
encodeUOp UOpLen = "len"
encodeUOp UOpMinus = "negate"
encodeUOp UOpNot = "not"

encodeMaybeType Nothing = Null
encodeMaybeType (Just t) = toJSON $ show t

writeScriptJSON :: FilePath -> Script -> Script -> IO ()
writeScriptJSON fname osc tsc =
    withOutputFile fname $ \hout ->
        BL.hPutStr hout $ encode $ object
                [ "header" .= toJSON (EncHeader <$> nglHeader osc)
                , "original-script" .= toJSON (second EncExpression <$> nglBody osc)
                , "transformed-script" .= toJSON (second EncExpression <$> nglBody tsc)
                ]
