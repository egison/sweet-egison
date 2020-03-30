{-# LANGUAGE TemplateHaskell #-}

module Data.Query.QQ
  ( q
  )
where

-- imports to create 'Name' in compilation
import           Data.Functor                   ( void )
import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Search           ( MonadSearch(..) )
import           Data.Query                     ( Query(..) )
import           Data.Query.Pattern             ( apply )

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
                                                , nameBase
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


q :: QuasiQuoter
q = QuasiQuoter { quoteExp  = compile
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


data TaggedTarget =
  TaggedTarget { proxy  :: Name
               , target :: Name
               }

compilePattern :: Pat.Expr Name Name Exp -> Exp -> Q Exp
compilePattern pat body = do
  tgt <- newTaggedTarget
  tr  <- go pat tgt
  pure . AppE (ConE queryName) . LamE [boundTag pat tgt, boundTgt pat tgt] $ tr
    s
 where
  s = AppE (VarE pureName) body
  go :: Pat.Expr Name Name Exp -> TaggedTarget -> Q (Exp -> Exp)
  go Pat.Wildcard _ = pure id
  go (Pat.Variable x) TaggedTarget { target } =
    pure $ \k -> let_ (VarP x) (VarE target) k
  go (Pat.Value e) t = pure $ AppE guardExp
   where
    guardExp = AppE
      (VarE guardedName)
      (AppE (VarE 'Data.Functor.void)
            (applyPat (AppE (VarE $ mkName "value") e) t)
      )
  go (Pat.Predicate e) TaggedTarget { target } = pure $ AppE guardExp
    where guardExp = AppE (VarE guardedName) (guard_ (AppE e (VarE target)))
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
  -- TODO: less ad-hoc optimization
  go (Pat.Infix n Pat.Wildcard p2) t | nameBase n == "join" =
    go (Pattern (mkName "spread") [p2]) t
  go (Pat.Infix n p1 p2) t = go (Pattern n [p1, p2]) t
  go (Pat.Collection ps) t = go (desugarCollection ps) t
  go (Pat.Tuple      ps) t = go (desugarTuple ps) t
  go (Pat.Pattern n ps ) t = do
    xs <- mapM (\p -> (p, ) <$> newTaggedTarget) ps
    tr <- foldrM go' id xs
    pure $ \k -> applyPat (VarE n) t
      `sbind_` LamE [TupP [expandTagPats xs, expandTgtPats xs]] (tr k)
   where
    go' (p, t') acc = do
      f <- go p t'
      pure $ f . acc
  newTaggedTarget = TaggedTarget <$> newName "tag" <*> newName "tgt"
  expandTgtPats   = TupP . map (uncurry boundTgt)
  expandTagPats   = TupP . map (uncurry boundTag)
  sbind_ x f = ParensE (UInfixE (ParensE x) (VarE sbindOp) (ParensE f))
  let_ p e1 = LetE [ValD p (NormalB e1) []]
  guard_ b =
    CondE b (AppE (VarE pureName) (TupE [])) (VarE 'Control.Monad.mzero)
  applyPat p TaggedTarget { proxy, target } = AppE
    (AppE (AppE (VarE 'Data.Query.Pattern.apply) p) (VarE proxy))
    (VarE target)
  queryName   = 'Data.Query.Query
  guardedName = 'Control.Monad.Search.guarded
  plusName    = 'Control.Monad.mplus
  pureName    = 'pure
  sbindOp     = '(Control.Monad.Search.>->)
  lnotName    = 'Control.Monad.Search.exclude
  pureU       = AppE (VarE pureName) (TupE [])

boundTag :: Pat.Expr Name Name Exp -> TaggedTarget -> Pat
boundTag p TaggedTarget { proxy } | shouldBind p = VarP proxy
                                  | otherwise    = WildP
 where
  shouldBind Pat.Wildcard      = False
  shouldBind (Pat.Predicate _) = False
  shouldBind (Pat.Variable  _) = False
  shouldBind (Pat.And a b    ) = shouldBind a || shouldBind b
  shouldBind (Pat.Or  a b    ) = shouldBind a || shouldBind b
  shouldBind (Pat.Not x      ) = shouldBind x
  shouldBind _                 = True

boundTgt :: Pat.Expr Name Name Exp -> TaggedTarget -> Pat
boundTgt p TaggedTarget { target } | shouldBind p = VarP target
                                   | otherwise    = WildP
 where
  shouldBind Pat.Wildcard  = False
  shouldBind (Pat.And a b) = shouldBind a || shouldBind b
  shouldBind (Pat.Or  a b) = shouldBind a || shouldBind b
  shouldBind (Pat.Not x  ) = shouldBind x
  shouldBind _             = True

desugarCollection :: [Pat.Expr Name Name Exp] -> Pat.Expr Name Name Exp
desugarCollection = foldr go $ Pat.Pattern (mkName "nil") []
  where go x acc = Pat.Pattern (mkName "cons") [x, acc]

desugarTuple :: [Pat.Expr Name Name Exp] -> Pat.Expr Name Name Exp
desugarTuple ps = Pat.Pattern (mkName name) ps
  where name = "tuple" ++ show (length ps)
