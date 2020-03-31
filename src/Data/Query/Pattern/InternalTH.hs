{-# LANGUAGE CPP #-}

module Data.Query.Pattern.InternalTH
  ( makeToTupleInstance
  , makeTuplePatternClass
  )
where

import           Control.Monad                  ( replicateM )
import           Language.Haskell.TH.Syntax     ( Q
                                                , Name
                                                , Dec
                                                , newName
                                                , mkName
                                                )
import           Language.Haskell.TH.Lib


makeToTupleInstance :: Int -> Q Dec
makeToTupleInstance n = do
  tys  <- genNames "a"
  vals <- genNames "x"
  makeWithNames tys vals
  where genNames x = replicateM n (newName x)

makeWithNames :: [Name] -> [Name] -> Q Dec
makeWithNames tyNames valNames = instanceD nullCxt instHead [typeDecl, valDecl]
 where
  nullCxt  = pure []
  instHead = appT (conT (mkName "ToTuple")) tyList
  tyList   = foldr (appT . appT promotedConsT . varT) promotedNilT tyNames
  tyTuple  = case tyNames of
    []  -> tupleT 0
    [x] -> varT x
    xs  -> foldl (\acc -> appT acc . varT) (tupleT (length tyNames)) xs
  valHListPat = foldr (\x acc -> conP (mkName "HCons") [varP x, acc])
                      (conP (mkName "HNil") [])
                      valNames
  valTuple = case valNames of
    []  -> tupE []
    [x] -> varE x
    xs  -> tupE $ map varE xs
#if MIN_VERSION_template_haskell(2,15,0)
  typeDecl =
    tySynInstD (tySynEqn Nothing (appT (conT (mkName "Tupled")) tyList) tyTuple)
#else
  typeDecl = tySynInstD (mkName "Tupled") (tySynEqn [tyList] tyTuple)
#endif
  valDecl = funD (mkName "tupled") [clause [valHListPat] (normalB valTuple) []]


-- | @makeTuplePatternClass n@ generates @Tuple*Pattern@ class for n-tuples.
makeTuplePatternClass :: Int -> Q Dec
makeTuplePatternClass n = classD nullCxt
                                 className
                                 [plainTV tagTyName, plainTV tgtTyName]
                                 []
                                 (nameDec : tagFamilyDecs ++ tgtFamilyDecs)
 where
  nullCxt      = pure []
  className    = mkName $ "Tuple" ++ show n ++ "Pattern"
  tagTyName    = mkName "t"
  tgtTyName    = mkName "a"
  tuplePatName = mkName $ "tuple" ++ show n
  elemTagTyName m = mkName $ "Tag" ++ show n ++ "_" ++ show m
  elemTgtTyName m = mkName $ "Target" ++ show n ++ "_" ++ show m
  tagFamilyDecs = map tagFamilyDec [1 .. n]
  tgtFamilyDecs = map tgtFamilyDec [1 .. n]
  tagFamilyDec m =
    openTypeFamilyD (elemTagTyName m) [plainTV tagTyName] noSig Nothing
  tgtFamilyDec m =
    openTypeFamilyD (elemTgtTyName m) [plainTV tgtTyName] noSig Nothing
  nameDec = sigD tuplePatName $ appT
    (appT (appT (appT (conT $ mkName "Pattern") (varT tagTyName)) (varT tgtTyName))
          tagTyList
    )
    tgtTyList
  tagTyList = foldr
    (\m -> appT
      (appT promotedConsT (appT (conT $ elemTagTyName m) (varT tagTyName)))
    )
    promotedNilT
    [1 .. n]
  tgtTyList = foldr
    (\m -> appT
      (appT promotedConsT (appT (conT $ elemTgtTyName m) (varT tgtTyName)))
    )
    promotedNilT
    [1 .. n]
