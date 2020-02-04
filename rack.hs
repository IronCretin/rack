{-# LANGUAGE LambdaCase #-}

import Data.Char
import Data.List
import Control.Monad.State.Strict
import Control.Applicative
import Data.Maybe
import qualified Data.Map.Strict as M

data Datum
    = Void
    | Nil
    | Cons { car, cdr :: Datum }
    | Int { ival :: Int }
    | Bool { bval :: Bool }
    | Symbol { sval :: String }
    -- | String { sval :: String }
    | WrapFun { name :: String, fval :: [Datum] -> Datum }
    -- | Fun { args :: [String], stmts :: [Datum] }

instance Show Datum where
    show Void          = "#<void>"
    show Nil           = "()"
    show (Cons h t)    = "(" ++ show h ++ slist t ++ ")"
    show (Int i)       = show i
    show (Bool True)   = "#t"
    show (Bool False)  = "#f"
    show (Symbol s)    = s
    -- show (String s)    = show s
    show (WrapFun n _) = "#<procedure:" ++ n ++ ">"

slist :: Datum -> String
slist Nil        = ""
slist (Cons h t) = " " ++ show h ++ slist t
slist d          = " . " ++ show d

isList :: Datum -> Bool
isList Nil        = True
isList (Cons _ t) = isList t
isList _          = False

fromList :: [Datum] -> Datum
fromList = foldr Cons Nil
toList :: Datum -> [Datum]
toList Nil        = []
toList (Cons h t) = h : toList t 

isNum :: Datum -> Bool
isNum (Int _) = True
isNum _       = False

data Token
    = TOpen Char
    | TClose Char
    | TInt Int
    | TSymb String
    | THash String
    | TDot
    | TSpace String
    deriving (Eq, Show)

isReserved :: Char -> Bool
isReserved c = isSpace c || isDigit c || c `elem` "()[]{}.';|\\"

tokenize :: String -> [Token]
tokenize []                            = []
tokenize l@(c:cs) | c `elem` "([{"     = TOpen c : tokenize cs
                  | c `elem` ")]}"     = TClose c : tokenize cs
                  | c == '.'           = TDot : tokenize cs
                  | isSpace c          = let (n, r) = span isSpace l in TSpace n : tokenize r
                  | isDigit c          = let (n, r) = span isDigit l in TInt (read n) : tokenize r
                  | c == ';'           = let (n, r) = break (== '\n') l in TSpace n : tokenize r
                  | c == '#'           = let (n, r) = break isReserved cs in THash n : tokenize r
                  | not $ isReserved c = let (n, r) = break isReserved l in TSymb n : tokenize r

detokenize :: [Token] -> String
detokenize ts = ts >>= \case 
    TOpen c  -> [c]
    TClose c -> [c]
    TInt i   -> show i
    TSymb s  -> s
    THash h  -> '#':h
    TDot     -> "."
    TSpace s -> s

close :: Char -> Char
close '(' = ')'
close '[' = ']'
close '{' = '}'

isTSpace :: Token -> Bool
isTSpace (TSpace _) = True
isTSpace _          = False

next :: MonadPlus m => StateT [t] m t
next = StateT $ \case
    x:xs -> pure (x, xs)
    []   -> empty

require :: (Eq t, MonadPlus m) => t -> StateT [t] m t
require x = mfilter (== x) next

-- space :: MonadPlus m => StateT [Token] m [Token]
-- space = many $ mfilter isTSpace next


dat :: StateT [Token] Maybe Datum
dat = StateT $ \case
    []             -> empty
    TOpen p:rest   -> flip runStateT rest $ do
        ds <- many dat
        end <- (require TDot *> dat) <|> pure Nil
        require (TClose (close p))
        pure $ foldr Cons end ds
    THash "t":rest -> pure (Bool True, rest)
    THash "f":rest -> pure (Bool False, rest)
    TSymb s:rest   -> pure (Symbol s, rest)
    TInt i:rest    -> pure (Int i, rest)
    _              -> empty

instance Read Datum where
    readsPrec _ s = maybeToList $ fmap (fmap detokenize) $ runStateT dat $ filter (not . isTSpace) $ tokenize s
    readList s = maybeToList $ fmap (fmap detokenize) $ runStateT (many dat) $ filter (not . isTSpace) $ tokenize s


-- evaluator :: Datum -> StateT (M.Map String Datum) IO Datum
