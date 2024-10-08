{
{-# LANGUAGE FlexibleContexts #-}

module Lexer (
  Token(..),
  scanTokens
) where

import Control.Monad.Except

}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  let                           { \p _ -> LET    p }
  in                            { \p _ -> IN     p }
  case                          { \p _ -> CASE   p }
  of                            { \p _ -> OF     p }
  $digit+                       { \p s -> NUM p (read s) }
  "->"                          { \p _ -> ARROW  p }
  \=                            { \p _ -> EQB    p }
  \\                            { \p _ -> LAM    p }
  [\+]                          { \p _ -> PLUS   p }
  \[                            { \p _ -> LBRAC  p }
  \]                            { \p _ -> RBRAC  p }
  \(                            { \p _ -> LPAREN p }
  \)                            { \p _ -> RPAREN p }
  \:                            { \p _ -> COLON  p }
  \;                            { \p _ -> SEMI   p }
  \,                            { \p _ -> COMMA  p }
  $alpha [$alpha $digit \_ \']* { \p s -> ID     p s }

{

data Token
  = LET    AlexPosn
  | IN     AlexPosn
  | EQB    AlexPosn
  | CASE   AlexPosn
  | OF     AlexPosn
  | PLUS   AlexPosn
  | LAM    AlexPosn
  | NUM    AlexPosn Int
  | ID     AlexPosn String
  | ARROW  AlexPosn
  | LPAREN AlexPosn
  | RPAREN AlexPosn
  | LBRAC  AlexPosn
  | RBRAC  AlexPosn
  | COLON  AlexPosn
  | COMMA  AlexPosn
  | SEMI   AlexPosn
  | EOF    AlexPosn
  deriving (Eq,Show)


getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _ lineNum _) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _ _ colNum) = colNum

scanTokens :: String -> Except String [Token]
scanTokens str = go (alexStartPos,'\n',[],str)
  where
    go inp@(pos,_,_,str) =
      case alexScan inp 0 of
        AlexEOF -> return []
        AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
        AlexSkip  inp' _       -> go inp'
        AlexToken inp' len act -> do
          res <- go inp'
          let rest = act pos (take len str)
          return (rest : res)

}
