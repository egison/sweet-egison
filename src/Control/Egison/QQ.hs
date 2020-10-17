-- |
--
-- Module:      Control.Egison.QQ
-- Description: Quasi-quoter to construct queries
-- Stability:   experimental
--
-- This module provides 'QuasiQuoter' that builds 'Query' from nice pattern expressions.

{-# LANGUAGE TemplateHaskell #-}

module Control.Egison.QQ
  ( mc
  , PP(..)
  )
where

-- imports to create 'Name' in compilation
import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Search           ( MonadSearch(..) )

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

-- | Quasi-quoter for pattern expressions.
mc :: QuasiQuoter
mc = QuasiQuoter { quoteExp  = compile
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
  mName <- newName "mat"
  tName <- newName "tgt"
  body' <- go pat mName tName (AppE (VarE 'pure) body)
  case pat of
    Pat.Wildcard   -> pure $ LamE [TupP [WildP, WildP]] body'
    Pat.Variable _ -> pure $ LamE [TupP [WildP, VarP tName]] body'
    _              -> pure $ LamE [TupP [VarP mName, VarP tName]] body'
 where
  let_ p e1 = LetE [ValD p (NormalB e1) []]
  sbind_ x f = ParensE (UInfixE (ParensE x) (VarE sbindOp) (ParensE f))
  plusName = 'Control.Monad.mplus
  sbindOp  = '(>>=)
  lnotName = 'Control.Monad.Search.lnot
  go :: Pat.Expr Name Name Exp -> Name -> Name -> Exp -> Q Exp
  go Pat.Wildcard     _ _     body = pure body
  go (Pat.Variable x) _ tName body = pure $ let_ (VarP x) (VarE tName) body
  go (Pat.Value e) mName tName body =
    pure
      $        AppE
                 (VarE 'fromList)
                 (AppE
                   (AppE (AppE (AppE (VarE (mkName "value")) e) (TupE [])) (VarE mName)
                   )
                   (VarE tName)
                 )
      `sbind_` LamE [TupP []] body
  go (Pat.Predicate e) _ tName body =
    pure
      $        AppE (VarE 'Control.Monad.Search.guard) (AppE e (VarE tName))
      `sbind_` LamE [TupP []] body
  go (Pat.And p1 p2) mName tName body =
    go p2 mName tName body >>= go p1 mName tName
  go (Pat.Or p1 p2) mName tName body = do
    r1 <- go p1 mName tName (AppE (VarE 'pure) (TupE []))
    r2 <- go p2 mName tName (AppE (VarE 'pure) (TupE []))
    pure $ AppE (AppE (VarE plusName) r1) r2 `sbind_` LamE [TupP []] body
  go (Pat.Not p) mName tName body = do
    r <- go p mName tName (AppE (VarE 'pure) (TupE []))
    pure $ AppE (VarE lnotName) r `sbind_` LamE [TupP []] body
  go (Pat.Collection ps) mName tName body =
    go (desugarCollection ps) mName tName body
  go (Pat.Tuple ps) mName tName body = go (desugarTuple ps) mName tName body
  -- PROBLEM: Ad-hoc optimization
  go (Pat.Infix c1 Pat.Wildcard (Pat.Infix c2 p Pat.Wildcard)) mName tName body
    | nameBase c1 == "join", nameBase c2 == "cons" = go
      (Pattern (mkName "elm") [p])
      mName
      tName
      body
  -- PROBLEM: Ad-hoc optimization
  go (Pat.Infix c1 p1 (Pat.Infix c2 p2 p3)) mName tName body
    | nameBase c1 == "join", nameBase c2 == "cons" = go
      (Pattern (mkName "joinCons") [p1, p2, p3])
      mName
      tName
      body
  go (Pat.Infix n p1 p2) mName tName body =
    go (Pattern n [p1, p2]) mName tName body
  go (Pat.Pattern cName []) mName tName body =
    pure
      $        AppE
                 (VarE 'fromList)
                 (AppE (AppE (AppE (VarE cName) (TupE [])) (VarE mName)) (VarE tName))
      `sbind_` LamE [TupP []] body
  go (Pat.Pattern cName [p]) mName tName body | isPatVar p = do
    mName' <- newName "tmpM"
    tName' <- newName "tmpT"
    let pp = toPP p
    body' <- go p mName' tName' body
    pure
      $        AppE
                 (VarE 'fromList)
                 (AppE (AppE (AppE (VarE cName) pp) (VarE mName)) (VarE tName))
      `sbind_` LamE [tNameToVar p tName'] body'
  go (Pat.Pattern cName [p]) mName tName body = do
    mName' <- newName "tmpM"
    tName' <- newName "tmpT"
    let pp = toPP p
    body' <- go p mName' tName' body
    pure
      $        let_
                 (mNameToVar p mName')
                 (AppE (AppE (VarE (mkName (show cName ++ "M"))) (VarE mName))
                       (VarE tName)
                 )
      $        AppE
                 (VarE 'fromList)
                 (AppE (AppE (AppE (VarE cName) pp) (VarE mName)) (VarE tName))
      `sbind_` LamE [tNameToVar p tName'] body'
  go (Pat.Pattern cName ps) mName tName body | all isPatVar ps = do
    mNames <- mapM (\_ -> newName "tmpM") ps
    tNames <- mapM (\_ -> newName "tmpT") ps
    let pps = map (Just . toPP) ps
    body' <- foldrM go' body (zip3 ps mNames tNames)
    pure
      $        AppE
                 (VarE 'fromList)
                 (AppE (AppE (AppE (VarE cName) (TupE pps)) (VarE mName)) (VarE tName))
      `sbind_` LamE [TupP (zipWith tNameToVar ps tNames)] body'
  go (Pat.Pattern cName ps) mName tName body = do
    mNames <- mapM (\_ -> newName "tmpM") ps
    tNames <- mapM (\_ -> newName "tmpT") ps
    let pps = map (Just . toPP) ps
    body' <- foldrM go' body (zip3 ps mNames tNames)
    pure
      $        let_
                 (TupP (zipWith mNameToVar ps mNames))
                 (AppE (AppE (VarE (mkName (show cName ++ "M"))) (VarE mName))
                       (VarE tName)
                 )
      $        AppE
                 (VarE 'fromList)
                 (AppE (AppE (AppE (VarE cName) (TupE pps)) (VarE mName)) (VarE tName))
      `sbind_` LamE [TupP (zipWith tNameToVar ps tNames)] body'
  go' :: (Pat.Expr Name Name Exp, Name, Name) -> Exp -> Q Exp
  go' (p, m, t) = go p m t
  isPatVar :: Pat.Expr Name Name Exp -> Bool
  isPatVar Pat.Wildcard      = True
  isPatVar (Pat.Variable  _) = True
  isPatVar (Pat.Predicate _) = True
  isPatVar _                 = False
  mNameToVar :: Pat.Expr Name Name Exp -> Name -> Pat
  mNameToVar Pat.Wildcard      _     = WildP
  mNameToVar (Pat.Variable  _) _     = WildP
  mNameToVar (Pat.Predicate _) _     = WildP
  mNameToVar _                 mName = VarP mName
  tNameToVar :: Pat.Expr Name Name Exp -> Name -> Pat
  tNameToVar Pat.Wildcard _     = WildP
  tNameToVar _            tName = VarP tName

desugarCollection :: [Pat.Expr Name Name Exp] -> Pat.Expr Name Name Exp
desugarCollection = foldr go $ Pat.Pattern (mkName "nil") []
  where go x acc = Pat.Pattern (mkName "cons") [x, acc]

desugarTuple :: [Pat.Expr Name Name Exp] -> Pat.Expr Name Name Exp
desugarTuple ps = Pat.Pattern (mkName name) ps
  where name = "tuple" ++ show (length ps)

data PP a = WC | VP a | GP

toPP :: Pat.Expr Name Name Exp -> Exp
toPP Pat.Wildcard  = ConE 'WC
toPP (Pat.Value e) = AppE (ConE 'VP) e
toPP _             = ConE 'GP
