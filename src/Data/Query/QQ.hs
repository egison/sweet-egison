{-# LANGUAGE TemplateHaskell #-}

module Data.Query.QQ
  ( query
  )
where

-- imports to create 'Name' in compilation
import           Control.Monad                  ( guard
                                                , mplus
                                                )
import           Control.Monad.Search           ( MonadSearch(..) )
import           Data.Query                     ( Query(..) )

-- main
import           Data.Maybe                     ( mapMaybe )
import           Text.Read                      ( readMaybe )
import           Data.Foldable                  ( foldrM )

import           Language.Haskell.TH            ( Q
                                                , Loc(..)
                                                , Exp(..)
                                                , Pat(..)
                                                , Dec(..)
                                                , Body(..)
                                                , Name
                                                , location
                                                , extsEnabled
                                                , newName
                                                , mkName
                                                )
import           Language.Haskell.TH.Quote      ( QuasiQuoter(..) )
import           Language.Haskell.TH           as TH
                                                ( Extension )
import           Language.Haskell.Meta.Syntax.Translate
                                                ( toExp )
import           Language.Haskell.Exts.Extension
                                                ( Extension(EnableExtension) )
import           Language.Haskell.Exts.Extension
                                               as Exts
                                                ( KnownExtension )
import           Language.Haskell.Exts.Parser   ( ParseResult(..)
                                                , defaultParseMode
                                                , parseExpWithMode
                                                )
import qualified Language.Haskell.Exts.Parser  as Exts
                                                ( ParseMode(..) )
import           Language.Egison.Syntax.Pattern
                                               as Pat
                                                ( Expr(..) )
import qualified Language.Egison.Parser.Pattern
                                               as Pat
                                                ( parseNonGreedy )
import           Language.Egison.Parser.Pattern ( Fixity(..)
                                                , ParseFixity(..)
                                                , Associativity(..)
                                                , Precedence(..)
                                                )
import           Language.Egison.Parser.Pattern.Mode.Haskell.TH
                                                ( ParseMode(..) )


query :: QuasiQuoter
query = QuasiQuoter { quoteExp  = compile
                    , quotePat  = undefined
                    , quoteType = undefined
                    , quoteDec  = undefined
                    }

listFixities :: [ParseFixity Name String]
listFixities =
  [ ParseFixity (Fixity AssocRight (Precedence 5) (mkName "join")) $ parser "++"
  , ParseFixity (Fixity AssocRight (Precedence 5) (mkName "cons")) $ parser ":"
  ]
 where
  parser symbol content | symbol == content = Right ()
                        | otherwise = Left $ show symbol ++ "is expected"

parseMode :: Q Exts.ParseMode
parseMode = do
  Loc { loc_filename } <- location
  extensions <- mapMaybe (fmap EnableExtension . convertExt) <$> extsEnabled
  pure defaultParseMode { Exts.parseFilename = loc_filename, Exts.extensions }
 where
  convertExt :: TH.Extension -> Maybe Exts.KnownExtension
  convertExt = readMaybe . show

parseExp :: Exts.ParseMode -> String -> Q Exp
parseExp mode content = case parseExpWithMode mode content of
  ParseOk x       -> pure $ toExp x
  ParseFailed _ e -> fail e

compile :: String -> Q Exp
compile content = do
  mode        <- parseMode
  (pat, rest) <- parsePatternExpr mode content
  bodySource  <- takeBody rest
  body        <- parseExp mode bodySource
  compilePattern pat body
 where
  takeBody ('-' : '>' : xs) = pure xs
  takeBody xs               = fail $ "\"->\" is expected, but found " ++ show xs

parsePatternExpr
  :: Exts.ParseMode -> String -> Q (Pat.Expr Name Name Exp, String)
parsePatternExpr haskellMode content = case Pat.parseNonGreedy mode content of
  Left  e -> fail $ show e
  Right x -> pure x
  where mode = ParseMode { haskellMode, fixities = Just listFixities }


compilePattern :: Pat.Expr Name Name Exp -> Exp -> Q Exp
compilePattern pat body = do
  tgt <- newName "tgt"
  tr  <- go pat tgt
  pure . AppE (ConE queryName) . LamE [VarP tgt] $ tr s
 where
  s = AppE (VarE pureName) body
  go :: Pat.Expr Name Name Exp -> Name -> Q (Exp -> Exp)
  go Pat.Wildcard     _ = pure id
  go (Pat.Variable x) t = pure $ \k -> let_ x (VarE t) k
  go (Pat.Value e) t =
    go (Pat.Predicate $ InfixE Nothing (VarE eqOp) (Just e)) t
  go (Pat.Predicate e) t = pure $ \k -> guardExp `sbind_` LamE [TupP []] k
    where guardExp = AppE (VarE guardName) (AppE e (VarE t))
  go (Pat.And p1 p2) t = do
    t1 <- go p1 t
    t2 <- go p2 t
    pure $ t1 . t2
  go (Pat.Or p1 p2) t = do
    t1 <- go p1 t
    t2 <- go p2 t
    pure $ \k ->
      AppE (AppE (VarE plusName) (t1 pureU)) (t2 pureU)
        `sbind_` LamE [TupP []] k
  go (Pat.Not p) t = do
    tr <- go p t
    pure $ \k -> AppE (VarE lnotName) (tr pureU) `sbind_` LamE [TupP []] k
  go (Pat.Infix n p1 p2) t = go (Pattern n [p1, p2]) t
  go (Pat.Collection ps) t = go (desugarCollection ps) t
  go (Pat.Tuple      ps) t = go (desugarTuple ps) t
  go (Pat.Pattern n ps ) t = do
    xs <- mapM (\p -> (p, ) <$> newName "d") ps
    tr <- foldrM go' id xs
    pure $ \k ->
      AppE (VarE n) (VarE t) `sbind_` LamE [TupP $ map (VarP . snd) xs] (tr k)
   where
    go' (p', t') acc = do
      f <- go p' t'
      pure $ f . acc
  sbind_ x f = ParensE (UInfixE (ParensE x) (VarE sbindOp) (ParensE f))
  let_ x e1 = LetE [ValD (VarP x) (NormalB e1) []]
  queryName = 'Data.Query.Query
  guardName = 'Control.Monad.guard
  plusName  = 'Control.Monad.mplus
  pureName  = 'pure
  eqOp      = '(==)
  sbindOp   = '(Control.Monad.Search.>->)
  lnotName  = 'Control.Monad.Search.exclude
  pureU     = AppE (VarE pureName) (TupE [])

desugarCollection :: [Pat.Expr Name Name Exp] -> Pat.Expr Name Name Exp
desugarCollection = foldr go $ Pat.Pattern (mkName "nil") []
  where go x acc = Pat.Pattern (mkName "cons") [x, acc]

desugarTuple :: [Pat.Expr Name Name Exp] -> Pat.Expr Name Name Exp
desugarTuple ps = Pat.Pattern (mkName name) ps
  where name = "tuple" ++ show (length ps)
