{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Tests.Variable
    ( tgroup_Variable
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Variable

tgroup_Variable = $(testGroupGenerator)

case_variable_compare = do
    assertBool "Compare Variables (==)" (mkVariable "Hello world" == mkVariable "Hello world")
    assertBool "Compare Variables (/=)" (mkVariable "Hello world" /= mkVariable "Goodbye")

    assertBool "Compare Variables (<)" $ let
            a = mkVariable "Hello world" 
            b = mkVariable "Goodbye"
        in (a < b) || (b < a)

    assertBool "Compare Variables (==, redux)" (mkVariable "Hello world" == mkVariable "Hello world")
    assertBool "varName . mkvariable == id" (varName (mkVariable "Hello world") == "Hello world")

