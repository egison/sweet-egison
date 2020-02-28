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
import           Control.Monad.Fail             ( MonadFail )

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
                                                )
import           Language.Haskell.TH.Quote      ( QuasiQuoter(..) )
import           Language.Haskell.TH           as TH
                                                ( Extension )
import           Language.Haskell.Meta.Syntax.Translate
                                                ( toExp )
import           Language.Haskell.Exts.Parser   ( ParseResult(..)
                                                , ParseMode(..)
                                                , defaultParseMode
                                                , parseExpWithMode
                                                )
import           Language.Haskell.Exts.Extension
                                                ( Extension(EnableExtension) )
import qualified Language.Haskell.Exts.Syntax  as Exts
                                                ( QName(UnQual)
                                                , Name(Ident)
                                                )
import           Language.Haskell.Exts.Extension
                                               as Exts
                                                ( KnownExtension )
import           Language.Egison.Syntax.Pattern
                                               as Pat
                                                ( Expr(..) )
import           Language.Egison.Parser.Pattern ( Fixity(..)
                                                , ParseFixity(..)
                                                , Associativity(..)
                                                , Precedence(..)
                                                )
import           Language.Egison.Parser.Pattern.Mode.Haskell.TH
                                               as Pat
                                                ( parseExprWithParseFixities )


query :: QuasiQuoter
query = QuasiQuoter { quoteExp  = compile
                    , quotePat  = undefined
                    , quoteType = undefined
                    , quoteDec  = undefined
                    }

parseMode :: Q ParseMode
parseMode = do
  Loc { loc_filename } <- location
  extensions <- mapMaybe (fmap EnableExtension . convertExt) <$> extsEnabled
  pure defaultParseMode { parseFilename = loc_filename, extensions }
 where
  convertExt :: TH.Extension -> Maybe Exts.KnownExtension
  convertExt = readMaybe . show

parseExp :: String -> Q Exp
parseExp content = do
  mode <- parseMode
  case parseExpWithMode mode content of
    ParseOk x       -> pure $ toExp x
    ParseFailed _ e -> fail e

compile :: String -> Q Exp
compile content = do
  (patSource, bodySource) <- splitClause content
  pat                     <- parsePatternExpr patSource
  body                    <- parseExp bodySource
  compilePattern pat body

splitClause :: MonadFail m => String -> m (String, String)
splitClause = go []
 where
  go acc ('=' : '>' : xs) = pure (acc, xs)
  go acc (x         : xs) = go (acc ++ [x]) xs
  go _   []               = fail "'=>' is expected, but not found"

listFixities :: [ParseFixity (Exts.QName ()) String]
listFixities =
  [ ParseFixity (Fixity AssocRight (Precedence 5) (uqName "join")) $ parser "++"
  , ParseFixity (Fixity AssocRight (Precedence 5) (uqName "cons")) $ parser ":"
  ]
 where
  uqName = Exts.UnQual () . Exts.Ident ()
  parser symbol content | symbol == content = Right ()
                        | otherwise = Left $ show symbol ++ "is expected"

parsePatternExpr :: String -> Q (Pat.Expr Name Name Exp)
parsePatternExpr content = do
  mode <- parseMode
  case Pat.parseExprWithParseFixities mode listFixities content of
    Left  e -> fail $ show e
    Right x -> pure x

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
