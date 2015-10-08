module Syntax where

import Context

data Term = TmTrue Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmVar Info Int Int -- the second Int means the length of context
          | TmAbs Info String TmType Term
          | TmApp Info Term Term
