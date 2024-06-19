{-# LANGUAGE RecordWildCards #-}

module Utils where

import Lexer

typeof :: Token -> String
typeof (IntL {..}) = "int"
typeof (Int {..}) = "int"
typeof (FloatL {..}) = "float"
typeof (Float {..}) = "float"
typeof (BoolL {..} ) = "bool"
typeof (Bool {..}) = "bool"
typeof (E {..}) = "error"
typeof _ = ""
