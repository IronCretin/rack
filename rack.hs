{-# LANGUAGE LambdaCase #-}

import Data.IORef
import Data.Char
import Data.List
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import Data.Map.Strict (Map)
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

newtype ChainMap k v = ChainMap { maps :: NonEmpty (Map k v) } deriving (Show, Eq)
chempty :: ChainMap k v
chempty = ChainMap $ pure M.empty
chinsert :: Ord k => k -> v -> ChainMap k v -> ChainMap k v
chinsert k v (ChainMap (m :| ms)) = ChainMap (M.insert k v m :| ms) 
chchange :: Ord k => k -> v -> ChainMap k v -> ChainMap k v
chchange k v (ChainMap ms) = ChainMap $ N.fromList $ go $ N.toList ms
    where go []                      =  []
          go (m:ms) | k `M.member` m = M.insert k v m : ms
                    | otherwise      = m : go ms
chlookup :: Ord k => k -> ChainMap k v -> Maybe v
chlookup k (ChainMap ms) = N.head <$> traverse (M.lookup k) ms
chpush :: ChainMap k v -> ChainMap k v
chpush (ChainMap ms) = ChainMap $ N.cons M.empty ms
chpop :: ChainMap k v -> ChainMap k v
chpop (ChainMap (m :| [])) = chempty
chpop (ChainMap (m :| m' : ms)) = ChainMap $ m :| ms


evaluator :: IORef (ChainMap String Datum) -> Datum -> IO Datum
evaluator envr (Symbol n) = do
    if n `elem` ["define"] then error "reserved word" else pure ()
    env <- readIORef envr
    case chlookup n env of
        Just d  -> pure d
        Nothing -> error $ "undefined variable: " ++ n ++ " not found in " ++ show env
evaluator envr (Cons h t) = case (h, t) of
    (Symbol "define", Cons (Symbol n) (Cons val Nil)) -> do
        val' <- evaluator envr val
        modifyIORef envr $ chinsert n val'
        pure Void
    _                                                 -> do
        fun <- evaluator envr h
        case fun of
            WrapFun _ f -> do
                args <- traverse (evaluator envr) (toList t)
                pure $ f args

evaluator envr dat        = pure dat

evalWith :: ChainMap String Datum -> [Datum] -> IO [Datum]
evalWith env dats = do
    envr <- newIORef env
    traverse (evaluator envr) dats

stdlib :: Map String Datum
stdlib = M.fromList $ [
    ("+", WrapFun "+" (Int . sum . fmap ival))
    ]

eval :: [Datum] -> IO [Datum]
eval = evalWith $ ChainMap $ pure stdlib