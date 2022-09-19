{-# LANGUAGE NamedFieldPuns #-}
module Token (Token) where

import TokenType (TokenType)

data Token a =
        Token { tokentype:: TokenType, lexeme :: String, literal :: a, line :: Int }

instance Show a => Show (Token a) where
        show (Token {tokentype, lexeme, literal }) = show tokentype ++ " " ++ show lexeme ++ " " ++ show literal
