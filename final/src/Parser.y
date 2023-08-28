{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Parser (
    parseExpr
  , parseTokens
  ) where

import Lexer
import Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    in    { IN _     }
    case  { CASE _   }
    of    { OF _     }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQB _    }
    '+'   { PLUS _   }
    ':'   { COLON _  }
    ';'   { SEMI _   }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
    '['   { LBRAC _  }
    ']'   { RBRAC _  }
    ','   { COMMA _  }


-- Operators
%right in
%nonassoc '=' case of
%left ';'
%right '->'
%right ':'
%left '+'
%%

Top  : ID '=' Expr                 { $3 }
     | Expr                        { $1 }

Expr : Expr ':' Expr                { EBin Cons  $1 $3 }
     | Expr '+'  Expr               { EBin Plus  $1 $3 }
     | '\\' ID '->' Expr            { ELam $2 $4       }
     | let ID '='  Expr in Expr     { ELet $2 $4 $6    }
     | let ID Ids '=' Expr in Expr  { ELet $2 (mkLam $3 $5) $7 }
     | case Expr of Cases           { ECase $2 $4      }
     | Axpr                         { $1               }

Axpr : Axpr Bxpr                   { EApp $1 $2       }
     | Bxpr                        { $1               }


Bxpr : TNUM                        { EInt $1        }
     | '(' Expr ')'                { $2             }
     | ID                          { EVar $1        }
     | '[' ']'                     { ENil           }
     | '[' Exprs ']'               { exprList $2    }

Exprs : Expr                       { [$1]           }
      | Expr ',' Exprs             { $1 : $3        }

Ids : ID                           { [$1]           }
    | ID Ids                       { $1 : $2        }

Pat : ID                           { PVar $1 }
    | TNUM                         { PInt $1      }
    | '[' ']'                      { PNil         }
    | Pat ':' Pat                  { PCons  $1 $3 }
    | '(' Pat ')'                  { $2           }

Case : Pat '->' Expr               { ($1, $3) }        

Cases : Case               { [$1] }
      | Case ';' Cases     { $1 : $3 }       

{
mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
