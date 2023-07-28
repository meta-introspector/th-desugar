{-# LANGUAGE TemplateHaskell, UnboxedTuples, ParallelListComp, CPP,
             RankNTypes, TypeFamilies,
             DataKinds, ConstraintKinds, PolyKinds, MultiParamTypeClasses,
             FlexibleInstances, ExistentialQuantification,
             ScopedTypeVariables, GADTs, ViewPatterns, TupleSections,
             TypeOperators, PartialTypeSignatures, PatternSynonyms,
             TypeApplications #-}
{-# OPTIONS -Wno-incomplete-patterns -Wno-overlapping-patterns
            -Wno-unused-matches -Wno-type-defaults
            -Wno-missing-signatures -Wno-unused-do-bind
            -Wno-missing-fields -Wno-incomplete-record-updates
            -Wno-partial-type-signatures -Wno-redundant-constraints #-}

#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
#endif

#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

#if __GLASGOW_HASKELL__ >= 809
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

#if __GLASGOW_HASKELL__ >= 906
{-# LANGUAGE TypeData #-}
#endif
{-# OPTIONS_GHC -ddump-to-file #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, UndecidableInstances, KindSignatures, DeriveGeneric, TypeApplications, DeriveAnyClass #-}

module Server.MetaCoq.TestMeta3 where
import Language.Haskell.TH.Syntax
--import Database.Persist.TH

-- oimport Data.Morpheus.Server -- .Types.GQLType
-- import Data.Morpheus.App
--import Data.Morpheus.Server.Types
--import GHC.Types
import Prelude(Show, pure, (.), show, Char, Maybe(..), Monad, return, map)
-- import Data.Morpheus.Server.Deriving.Utils.GRep
import Server.MetaCoq.TestMeta
--import Test.Tasty
--import Test.Tasty.HUnit
import Data.String
-- import Data.Generics.Traversable
-- import Data.Generics.Traversable.Zipper
-- import Data.Generics.Traversable.TH
import Server.MetaCoq.TestMeta2
--import Data.ByteString.Lazy.Char8 (ByteString)
--import Data.Morpheus (interpreter)
--import Data.Morpheus.Document (importGQLDocument)
--import Data.Morpheus.Types (RootResolver (..), Undefined, ID, GQLType, QUERY,
--                            ResolverQ,
--                            Resolver)
import           GHC.Generics
import Data.Text (Text)
--import Splices
-- import qualified DsDec
-- import qualified Dec
-- import Dec ( RecordSel )
-- import ReifyTypeCUSKs
-- import ReifyTypeSigs
import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OSet as OS
import Language.Haskell.TH.Desugar.Expand  ( expandUnsoundly )
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as Syn

import Control.Exception ( ErrorCall )
import Control.Monad

import qualified Data.Map as M
import Data.Proxy

#if __GLASGOW_HASKELL__ >= 900
import Prelude as P
#endif

#if __GLASGOW_HASKELL__ >= 906
import GHC.Tuple ( Solo(MkSolo) )
#elif __GLASGOW_HASKELL__ >= 900
import GHC.Tuple ( Solo(Solo) )
#endif

#if __GLASGOW_HASKELL__ >= 908
import qualified FakeTuples
import GHC.Tuple ( Tuple0, Tuple1, Tuple2, Tuple3, Unit )
#endif

--class U a
--instance U a
-- instance GTraversable c0 (TestMeta.Prod TestMeta.Global_env TestMeta.Term)
--mshow :: d -> Char
--mshow x = 'c'

--  foo1 :: String
-- foo1 = 
-- ==  gfoldMap @Show (pure . show) rec_def_term  -- :: [String]--[GHC.Types.Char]
-- data Query (m :: * -> *) = Query
--   { globals :: ID -> m (Maybe (Prod Global_env Term))
--   } deriving (Generic
--              --, GQLType
--              )

--globalsResolver :: Monad m => ID -> Resolver QUERY m (Maybe (Prod Global_env Term))
--globalsResolver id = pure (Just (rec_def_term id))

--globalsResolver :: ID -> ResolverQ e Haxl (Prod Global_env Term)
--globalsResolver id = return (Just rec_def_term)

--(GHC.Generics.Rep (Prod Global_env Term)))

  -- hidden Data.Morpheus.Server.Deriving.Utils.GRep.GRep
-- foo =   GQLType (
--       (Resolver
--       (QUERY      ()       IO)
--         (Resolver QUERY   () IO
--          (ResolverValue (Resolver QUERY ()  IO)))
--         (GHC.Generics.Rep (Prod Global_env Term))



--   Global_declarations

--  (List (Branch Term)) 
--  (List (Context_decl Term)) 
--  (List (Def Term))
--  (List Aname) 
--  (List Constructor_body) 
--  (List Elt1) 
--  (List Ident) 
--  (List One_inductive_body) 
--  (List Projection_body) 
--  (List T3) 
--  (List T36) 
--  (List Term) 

--  (Option (List T36)) 
--  (Option Kername) 
--  (Option Term) 
--  (Predicate Term)

--data Prod a b =
--    Pair a b

--  (Prod Global_env Bool) 
--  (Prod Global_env Term)
--  (Prod Kername Global_decl) 

--data Modpath =
--   MPfile Dirpath
-- | MPbound Dirpath Ident Nat
-- | MPdot Modpath Ident

--  (Prod Modpath Ident) 

--  (Prod T10 T32) 
-- type T10 = T_0
-- type T35 = Prod T10 T32
-- type T32 = T_4
-- type T_4 = T26
-- type T26 = Tree0
--data Tree0 =
--   Leaf0
-- | Node0 T0 Tree0 (Prod (Prod T3 T24) T3) Tree0

--  (Prod T3 Nat)

--  (Prod T3 T24)

-- type T24 = T_3
-- (Prod (Prod T3 T24) T3)
-- type Elt4 = Prod (Prod T3 T24) T3
-- type T25 = Prod (Prod T3 T24) T3

-- type BigMama = (ProdProd Global_env Term)

-- type Aname = Binder_annot Name
-- type Binder_annot  = BinderAnnot
--   MkBindAnn a Relevance

-- type Case_info = CaseInfo
-- Mk_case_info Inductive Nat Relevance

-- type Cast_kind = CastKind
-- type Context = List (Context_decl Term)
-- type Dirpath = List Ident
-- type Elt0 = T_
-- type Elt1 = Prod T3 Nat
-- type Elt2 = Prod T3 Nat

-- type Global_declarations = List (Prod Kername Global_decl)

-- type Ident = MyString
--   EmptyString
-- | String Byte MyString


-- type Kername = Prod Modpath Ident

-- type Mfixpoint term = List (Def term)
-- type Mutual_inductive_body = MutualInductiveBody
-- type NonEmptyLevelExprSet =   T21
-- type Recursivity_kind = RecursivityKind
-- type T =MyString
-- type T0 = Z

-- type T14 = Prod T3 Nat
-- type T18 = List Elt1
-- type T21 = T_1
-- type T22 = NonEmptyLevelExprSet
-- type T23 = T_2



-- type T3 = T_

-- type T33 = List T3

-- type T4 = Tree
-- type T_0 = T4
-- type T_1 = T18

-- type Universes_decl = UniversesDecl

step1 :: Global_declarations
step1 = case rec_def_term of { Pair (Mk_global_env _ a _ ) _ -> a }

step2 :: TGlobalDeclaration
step2 = case step1 of { Cons a b -> a }

step3 :: Global_decl
step3 = case step2 of { Server.MetaCoq.TestMeta.Pair a b  -> b }

step3a :: Kername
step3a = case step2 of { Server.MetaCoq.TestMeta.Pair a b  -> a }

get_file :: Prod Modpath b -> Dirpath
get_file x = case x of {
  Pair (MPdot(MPfile(a)) _) _ -> a;
    -- Pair (MPdot (MPbound _ _ _) EmptyString) _
    --   Pair (MPdot (MPbound _ _ _) (Server.MetaCoq.TestMeta.String _ _)) _
    --  Pair (MPdot (MPdot _ _) EmptyString) _
    --  Pair (MPdot (MPdot _ _) (Server.MetaCoq.TestMeta.String _ _)) _

  }

step4_inductive :: Mutual_inductive_body
step4_inductive = step_inductive step3

step_inductive :: Global_decl -> Mutual_inductive_body
step_inductive s = case s of { InductiveDecl a -> a }

step5_inductive4 :: List One_inductive_body
step5_inductive4 = case step4_inductive of { Build_mutual_inductive_body _ _ _ a  _ _  -> a}

step6_name_of_type :: String
step6_name_of_type = case step5_inductive4 of { (Cons (Build_one_inductive_body aIdent aContext aT23 aTerm aAllowed_eliminations _ _ _) Nil)  -> concatChars aIdent }


-- derivePersistField "Server.MetaCoq.TestMeta.Byte"
decls = --mapM dsType
  [
  ''Server.MetaCoq.TestMeta.Aname 
  ,''Server.MetaCoq.TestMeta.BigMama 
  ,''Server.MetaCoq.TestMeta.Binder_annot
  ,''Server.MetaCoq.TestMeta.BinderAnnotName 
  ,''Server.MetaCoq.TestMeta.BranchTerm 
  ,''Server.MetaCoq.TestMeta.Case_info 
  ,''Server.MetaCoq.TestMeta.Cast_kind 
  ,''Server.MetaCoq.TestMeta.Context 
  ,''Server.MetaCoq.TestMeta.Context_declTerm 
  ,''Server.MetaCoq.TestMeta.DefTerm 
  ,''Server.MetaCoq.TestMeta.Dirpath 
  ,''Server.MetaCoq.TestMeta.Elt0 
  ,''Server.MetaCoq.TestMeta.Elt1 
  ,''Server.MetaCoq.TestMeta.Elt2 
  ,''Server.MetaCoq.TestMeta.Global_declarations 
  ,''Server.MetaCoq.TestMeta.Ident 
  ,''Server.MetaCoq.TestMeta.Kername 
  ,''Server.MetaCoq.TestMeta.ListAname 
  ,''Server.MetaCoq.TestMeta.ListBranchTerm 
  ,''Server.MetaCoq.TestMeta.ListConstructor_body 
  ,''Server.MetaCoq.TestMeta.ListContext_declTerm 
  ,''Server.MetaCoq.TestMeta.ListDefTerm 
  ,''Server.MetaCoq.TestMeta.ListElt1 
  ,''Server.MetaCoq.TestMeta.ListOne_inductive_body 
  ,''Server.MetaCoq.TestMeta.ListProjection_body 
  ,''Server.MetaCoq.TestMeta.ListT3 
  ,''Server.MetaCoq.TestMeta.ListT36 
  ,''Server.MetaCoq.TestMeta.ListTerm 
  ,''Server.MetaCoq.TestMeta.Mfixpoint
  ,''Server.MetaCoq.TestMeta.Mutual_inductive_body 
  ,''Server.MetaCoq.TestMeta.NonEmptyLevelExprSet 
  ,''Server.MetaCoq.TestMeta.OKername 
  ,''Server.MetaCoq.TestMeta.OptionListT36 
  ,''Server.MetaCoq.TestMeta.OptionTerm 
  ,''Server.MetaCoq.TestMeta.PredicateTerm 
  ,''Server.MetaCoq.TestMeta.Recursivity_kind 
  ,''Server.MetaCoq.TestMeta.T 
  ,''Server.MetaCoq.TestMeta.T_0 
  ,''Server.MetaCoq.TestMeta.T0 
  ,''Server.MetaCoq.TestMeta.T_1 
  ,''Server.MetaCoq.TestMeta.T10 
  ,''Server.MetaCoq.TestMeta.T14 
  ,''Server.MetaCoq.TestMeta.T18 
  ,''Server.MetaCoq.TestMeta.T21 
  ,''Server.MetaCoq.TestMeta.T22 
  ,''Server.MetaCoq.TestMeta.T23 
  ,''Server.MetaCoq.TestMeta.T24 
  ,''Server.MetaCoq.TestMeta.T25 
  ,''Server.MetaCoq.TestMeta.T25S 
  ,''Server.MetaCoq.TestMeta.T26 
  ,''Server.MetaCoq.TestMeta.T3 
  ,''Server.MetaCoq.TestMeta.T32 
  ,''Server.MetaCoq.TestMeta.T33 
  ,''Server.MetaCoq.TestMeta.T35 
  ,''Server.MetaCoq.TestMeta.T_4 
  ,''Server.MetaCoq.TestMeta.T4 
  ,''Server.MetaCoq.TestMeta.TGlobalDeclaration 
  ,''Server.MetaCoq.TestMeta.Universes_decl 
  ]


--fooList [] = []
--fooList (x:xs) = Language.Haskell.TH.Syntax.reify (mkName x):fooList xs
fooList :: [String] -> [Q Info]
fooList = map (Language.Haskell.TH.Syntax.reify . mkName) 

--foo1 ::Syn.Name -> Int -> Int
--foo1 = 1

test_t100 x =
  do mapM dsReify [
         ''Server.MetaCoq.TestMeta.Aname 
         ,''Server.MetaCoq.TestMeta.BigMama 
         ,''Server.MetaCoq.TestMeta.Binder_annot
         ,''Server.MetaCoq.TestMeta.BinderAnnotName 
         ,''Server.MetaCoq.TestMeta.BranchTerm 
         ,''Server.MetaCoq.TestMeta.Case_info 
         ,''Server.MetaCoq.TestMeta.Cast_kind 
         ,''Server.MetaCoq.TestMeta.Context 
         ,''Server.MetaCoq.TestMeta.Context_declTerm 
         ,''Server.MetaCoq.TestMeta.DefTerm 
         ,''Server.MetaCoq.TestMeta.Dirpath 
         ,''Server.MetaCoq.TestMeta.Elt0 
         ,''Server.MetaCoq.TestMeta.Elt1 
         ,''Server.MetaCoq.TestMeta.Elt2 
         ,''Server.MetaCoq.TestMeta.Global_declarations 
         ,''Server.MetaCoq.TestMeta.Ident 
         ,''Server.MetaCoq.TestMeta.Kername 
         ,''Server.MetaCoq.TestMeta.ListAname 
         ,''Server.MetaCoq.TestMeta.ListBranchTerm 
         ,''Server.MetaCoq.TestMeta.ListConstructor_body 
         ,''Server.MetaCoq.TestMeta.ListContext_declTerm 
         ,''Server.MetaCoq.TestMeta.ListDefTerm 
         ,''Server.MetaCoq.TestMeta.ListElt1 
         ,''Server.MetaCoq.TestMeta.ListOne_inductive_body 
         ,''Server.MetaCoq.TestMeta.ListProjection_body 
         ,''Server.MetaCoq.TestMeta.ListT3 
         ,''Server.MetaCoq.TestMeta.ListT36 
         ,''Server.MetaCoq.TestMeta.ListTerm 
         ,''Server.MetaCoq.TestMeta.Mfixpoint
         ,''Server.MetaCoq.TestMeta.Mutual_inductive_body 
         ,''Server.MetaCoq.TestMeta.NonEmptyLevelExprSet 
         ,''Server.MetaCoq.TestMeta.OKername 
         ,''Server.MetaCoq.TestMeta.OptionListT36 
         ,''Server.MetaCoq.TestMeta.OptionTerm 
         ,''Server.MetaCoq.TestMeta.PredicateTerm 
         ,''Server.MetaCoq.TestMeta.Recursivity_kind 
         ,''Server.MetaCoq.TestMeta.T 
         ,''Server.MetaCoq.TestMeta.T_0 
         ,''Server.MetaCoq.TestMeta.T0 
         ,''Server.MetaCoq.TestMeta.T_1 
         ,''Server.MetaCoq.TestMeta.T10 
         ,''Server.MetaCoq.TestMeta.T14 
         ,''Server.MetaCoq.TestMeta.T18 
         ,''Server.MetaCoq.TestMeta.T21 
         ,''Server.MetaCoq.TestMeta.T22 
         ,''Server.MetaCoq.TestMeta.T23 
         ,''Server.MetaCoq.TestMeta.T24 
         ,''Server.MetaCoq.TestMeta.T25 
         ,''Server.MetaCoq.TestMeta.T25S 
         ,''Server.MetaCoq.TestMeta.T26 
         ,''Server.MetaCoq.TestMeta.T3 
         ,''Server.MetaCoq.TestMeta.T32 
         ,''Server.MetaCoq.TestMeta.T33 
         ,''Server.MetaCoq.TestMeta.T35 
         ,''Server.MetaCoq.TestMeta.T_4 
         ,''Server.MetaCoq.TestMeta.T4 
         ,''Server.MetaCoq.TestMeta.TGlobalDeclaration 
         ,''Server.MetaCoq.TestMeta.Universes_decl 
         
         ] 

--  $(stringE . show =<< do; let f <- case Server.MetaCoq.TestMeta3.decls of (x:xs) -> return 
--  ++ $(stringE . show =<< Language.Haskell.TH.Syntax.reify (mkName "Server.MetaCoq.TestMeta.Aname"))

--getDesugaredStrings :: ModuleName -> Q Exp  
-- getDesugaredStrings moduleName = do

--   -- Get list of names to reify
--   names <- lookupValueSymbols moduleName 
  
--   -- Reify each name
--   infos <- mapM (dsReify . mkName . symbolName) names
  
--   -- Pretty print each desugared DInfo 
--   strings <- mapM (fmap show . lift) infos

--   -- Build list 
--   result <- listE strings

--   -- Lift and splice result
--   return  $(lift result)

-- --prettyPrintInfo :: DInfo -> String
-- --prettyPrintInfo info = ...
-- --getDesugaredStrings2 :: ModuleName -> Q Exp
-- getDesugaredStrings2 = do
--   decls1 <- [
--          ''Server.MetaCoq.TestMeta.Aname 
--          ,''Server.MetaCoq.TestMeta.BigMama 
--          ,''Server.MetaCoq.TestMeta.Binder_annot
--          ,''Server.MetaCoq.TestMeta.BinderAnnotName 
--          ,''Server.MetaCoq.TestMeta.BranchTerm 
--          ,''Server.MetaCoq.TestMeta.Case_info 
--          ,''Server.MetaCoq.TestMeta.Cast_kind 
--          ,''Server.MetaCoq.TestMeta.Context 
--          ,''Server.MetaCoq.TestMeta.Context_declTerm 
--          ,''Server.MetaCoq.TestMeta.DefTerm 
--          ,''Server.MetaCoq.TestMeta.Dirpath 
--          ,''Server.MetaCoq.TestMeta.Elt0 
--          ,''Server.MetaCoq.TestMeta.Elt1 
--          ,''Server.MetaCoq.TestMeta.Elt2 
--          ,''Server.MetaCoq.TestMeta.Global_declarations 
--          ,''Server.MetaCoq.TestMeta.Ident 
--          ,''Server.MetaCoq.TestMeta.Kername 
--          ,''Server.MetaCoq.TestMeta.ListAname 
--          ,''Server.MetaCoq.TestMeta.ListBranchTerm 
--          ,''Server.MetaCoq.TestMeta.ListConstructor_body 
--          ,''Server.MetaCoq.TestMeta.ListContext_declTerm 
--          ,''Server.MetaCoq.TestMeta.ListDefTerm 
--          ,''Server.MetaCoq.TestMeta.ListElt1 
--          ,''Server.MetaCoq.TestMeta.ListOne_inductive_body 
--          ,''Server.MetaCoq.TestMeta.ListProjection_body 
--          ,''Server.MetaCoq.TestMeta.ListT3 
--          ,''Server.MetaCoq.TestMeta.ListT36 
--          ,''Server.MetaCoq.TestMeta.ListTerm 
--          ,''Server.MetaCoq.TestMeta.Mfixpoint
--          ,''Server.MetaCoq.TestMeta.Mutual_inductive_body 
--          ,''Server.MetaCoq.TestMeta.NonEmptyLevelExprSet 
--          ,''Server.MetaCoq.TestMeta.OKername 
--          ,''Server.MetaCoq.TestMeta.OptionListT36 
--          ,''Server.MetaCoq.TestMeta.OptionTerm 
--          ,''Server.MetaCoq.TestMeta.PredicateTerm 
--          ,''Server.MetaCoq.TestMeta.Recursivity_kind 
--          ,''Server.MetaCoq.TestMeta.T 
--          ,''Server.MetaCoq.TestMeta.T_0 
--          ,''Server.MetaCoq.TestMeta.T0 
--          ,''Server.MetaCoq.TestMeta.T_1 
--          ,''Server.MetaCoq.TestMeta.T10 
--          ,''Server.MetaCoq.TestMeta.T14 
--          ,''Server.MetaCoq.TestMeta.T18 
--          ,''Server.MetaCoq.TestMeta.T21 
--          ,''Server.MetaCoq.TestMeta.T22 
--          ,''Server.MetaCoq.TestMeta.T23 
--          ,''Server.MetaCoq.TestMeta.T24 
--          ,''Server.MetaCoq.TestMeta.T25 
--          ,''Server.MetaCoq.TestMeta.T25S 
--          ,''Server.MetaCoq.TestMeta.T26 
--          ,''Server.MetaCoq.TestMeta.T3 
--          ,''Server.MetaCoq.TestMeta.T32 
--          ,''Server.MetaCoq.TestMeta.T33 
--          ,''Server.MetaCoq.TestMeta.T35 
--          ,''Server.MetaCoq.TestMeta.T_4 
--          ,''Server.MetaCoq.TestMeta.T4 
--          ,''Server.MetaCoq.TestMeta.TGlobalDeclaration 
--          ,''Server.MetaCoq.TestMeta.Universes_decl          
--          ] 
-- --  infos <- mapM (dsReify . mkName ) decls1
--   infos <- map show (decls1)
--   strings <- mapM (fmap show . lift) infos
--   let result = listE strings
--   join (lift result)
--   --infos <- mapM (dsReify . mkName . nameBase . nameName) decls
