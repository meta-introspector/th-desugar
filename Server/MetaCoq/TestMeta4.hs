{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
--         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T_0 [] (DConT ''Server.MetaCoq.TestMeta.T4)) Nothing),
module Server.MetaCoq.TestMeta4 where
import Language.Haskell.TH.Syntax
import GHC.Types

-- $(stringE . show =<<  reify 'test_t100)
-- "VarI Server.MetaCoq.TestMeta3.test_t100 (ForallT [KindedTV m_7566047373983534213 InferredSpec (AppT (AppT ArrowT StarT) StarT),KindedTV p_7566047373983534214 InferredSpec StarT] [AppT (ConT Language.Haskell.TH.Desugar.Reify.DsMonad) (VarT m_7566047373983534213)] (AppT (AppT ArrowT (VarT p_7566047373983534214)) (AppT (VarT m_7566047373983534213) (AppT ListT (AppT (ConT GHC.Maybe.Maybe) (ConT Language.Haskell.TH.Desugar.AST.DInfo)))))) Nothing"

-- $(stringE . show =<< (sequence foo1))
import Server.MetaCoq.TestMeta

import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.AST

--term_7566047373982786669 = mkName "foo1"

decls = [Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Aname [] (DAppT (DConT ''Server.MetaCoq.TestMeta.Binder_annot) (DConT ''Server.MetaCoq.TestMeta.Name))) Nothing),
         Just (DTyConI
               (DTySynD ''Server.MetaCoq.TestMeta.BigMama []
                (DAppT
                 (DAppT (DConT ''Server.MetaCoq.TestMeta.Prod)
                 (DConT ''Server.MetaCoq.TestMeta.Global_env))
                 (DConT ''Server.MetaCoq.TestMeta.Term)))
                Nothing),
         
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Binder_annot [] (DConT ''Server.MetaCoq.TestMeta.BinderAnnot)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.BinderAnnotName [] (DAppT (DConT ''Server.MetaCoq.TestMeta.BinderAnnot) (DConT ''Server.MetaCoq.TestMeta.Name))) Nothing),Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.BranchTerm [] (DAppT (DConT ''Server.MetaCoq.TestMeta.Branch) (DConT ''Server.MetaCoq.TestMeta.Term))) Nothing),Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Case_info [] (DConT ''Server.MetaCoq.TestMeta.CaseInfo)) Nothing),Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Cast_kind [] (DConT ''Server.MetaCoq.TestMeta.CastKind)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Context [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DAppT (DConT ''Server.MetaCoq.TestMeta.Context_decl) (DConT ''Server.MetaCoq.TestMeta.Term)))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Context_declTerm [] (DAppT (DConT ''Server.MetaCoq.TestMeta.Context_decl) (DConT ''Server.MetaCoq.TestMeta.Term))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.DefTerm [] (DAppT (DConT ''Server.MetaCoq.TestMeta.Def) (DConT ''Server.MetaCoq.TestMeta.Term))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Dirpath [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.Ident))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Elt0 [] (DConT ''Server.MetaCoq.TestMeta.T_)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Elt1 [] (DConT ''Server.MetaCoq.TestMeta.T14)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Elt2 [] (DConT ''Server.MetaCoq.TestMeta.T14)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Global_declarations [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.TGlobalDeclaration))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Ident [] (DConT ''Server.MetaCoq.TestMeta.MyString)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Kername [] (DAppT (DAppT (DConT ''Server.MetaCoq.TestMeta.Prod) (DConT ''Server.MetaCoq.TestMeta.Modpath)) (DConT ''Server.MetaCoq.TestMeta.Ident))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.ListAname [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.Aname))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.ListBranchTerm [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.BranchTerm))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.ListConstructor_body [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.Constructor_body))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.ListContext_declTerm [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.Context_declTerm))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.ListDefTerm [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.DefTerm))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.ListElt1 [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.Elt1))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.ListOne_inductive_body [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.One_inductive_body))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.ListProjection_body [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.Projection_body))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.ListT3 [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.T3))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.ListT36 [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.T36))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.ListTerm [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.Term))) Nothing),
         
         -- Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Mfixpoint [
         --                   DKindedTV term_7566047373982786669 () (
         --                       DConT ''GHC.Types.Type
         --                       )
         --                   ] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DAppT (DConT ''Server.MetaCoq.TestMeta.Def) (DVarT term_7566047373982786669)))) Nothing),
          
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Mutual_inductive_body [] (DConT ''Server.MetaCoq.TestMeta.MutualInductiveBody)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.NonEmptyLevelExprSet [] (DConT ''Server.MetaCoq.TestMeta.T21)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.OKername [] (DAppT (DConT ''Server.MetaCoq.TestMeta.Option) (DConT ''Server.MetaCoq.TestMeta.Kername))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.OptionListT36 [] (DAppT (DConT ''Server.MetaCoq.TestMeta.Option) (DConT ''Server.MetaCoq.TestMeta.ListT36))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.OptionTerm [] (DAppT (DConT ''Server.MetaCoq.TestMeta.Option) (DConT ''Server.MetaCoq.TestMeta.Term))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.PredicateTerm [] (DAppT (DConT ''Server.MetaCoq.TestMeta.Predicate) (DConT ''Server.MetaCoq.TestMeta.Term))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Recursivity_kind [] (DConT ''Server.MetaCoq.TestMeta.RecursivityKind)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T [] (DConT ''Server.MetaCoq.TestMeta.MyString)) Nothing),

         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T0 [] (DConT ''Server.MetaCoq.TestMeta.Z)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T_1 [] (DConT ''Server.MetaCoq.TestMeta.T18)) Nothing),

         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T14 [] (DAppT (DAppT (DConT ''Server.MetaCoq.TestMeta.Prod) (DConT ''Server.MetaCoq.TestMeta.T3)) (DConT ''Server.MetaCoq.TestMeta.Nat))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T18 [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.Elt1))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T21 [] (DConT ''Server.MetaCoq.TestMeta.T_1)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T22 [] (DConT ''Server.MetaCoq.TestMeta.NonEmptyLevelExprSet)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T23 [] (DConT ''Server.MetaCoq.TestMeta.T_2)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T24 [] (DConT ''Server.MetaCoq.TestMeta.T_3)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T25 [] (DAppT (DAppT (DConT ''Server.MetaCoq.TestMeta.Prod) (DConT ''Server.MetaCoq.TestMeta.T25S)) (DConT ''Server.MetaCoq.TestMeta.T3))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T25S [] (DAppT (DAppT (DConT ''Server.MetaCoq.TestMeta.Prod) (DConT ''Server.MetaCoq.TestMeta.T3)) (DConT ''Server.MetaCoq.TestMeta.T24))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T26 [] (DConT ''Server.MetaCoq.TestMeta.Tree0)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T3 [] (DConT ''Server.MetaCoq.TestMeta.T_)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T32 [] (DConT ''Server.MetaCoq.TestMeta.T_4)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T33 [] (DAppT (DConT ''Server.MetaCoq.TestMeta.List) (DConT ''Server.MetaCoq.TestMeta.T3))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T35 [] (DAppT (DAppT (DConT ''Server.MetaCoq.TestMeta.Prod) (DConT ''Server.MetaCoq.TestMeta.T10)) (DConT ''Server.MetaCoq.TestMeta.T32))) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T_4 [] (DConT ''Server.MetaCoq.TestMeta.T26)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T4 [] (DConT ''Server.MetaCoq.TestMeta.Tree)) Nothing),
         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.TGlobalDeclaration [] (DAppT (DAppT (DConT ''Server.MetaCoq.TestMeta.Prod) (DConT ''Server.MetaCoq.TestMeta.Kername)) (DConT ''Server.MetaCoq.TestMeta.Global_decl))) Nothing),
         Just  (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.Universes_decl [] (DConT ''Server.MetaCoq.TestMeta.UniversesDecl)) Nothing)
        ]
--         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T35 [] (DAppT (DAppT (DConT ''Server.MetaCoq.TestMeta.Prod) (DConT ''Server.MetaCoq.TestMeta.T10)) (DConT ''Server.MetaCoq.TestMeta.T32))) Nothing),
--         Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T10 [] (DConT ''Server.MetaCoq.TestMeta.T_0)) Nothing),
--           Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T_0 [] (DConT ''Server.MetaCoq.TestMeta.T4)) Nothing),
--           Just (DTyConI (DTySynD ''Server.MetaCoq.TestMeta.T4 [] (DConT ''Server.MetaCoq.TestMeta.Tree)) Nothing),




--ghci> $(stringE . show =<<  reify 'Server.MetaCoq.TestMeta4.decls)
--"VarI Server.MetaCoq.TestMeta4.decls (AppT ListT (AppT (ConT GHC.Maybe.Maybe) (ConT Language.Haskell.TH.Desugar.AST.DInfo))) Nothing"

getStringFromName :: DsMonad [] => Language.Haskell.TH.Syntax.Name -> String--q (Maybe DInfo)
getStringFromName name = do
  info <- (dsReify name)
  case info of
    --   = DTyConI DDec (Maybe [DInstanceDec])
    Just( a) -> "OK"
    _ -> "nope"
  where
--    getStr2 (DDeclaredInfix a) = getStr2 a
    getStr3 _ = "nada"
    getStr3 (DDataD _ _ name _ _ _ _) = (nameBase name)
      -- DLetDec DLetDec
      -- | DDataD DataFlavor
      --   DCxt
      --   Name
      --   [DTyVarBndrUnit]
      --   (Maybe DKind)
      --   [DCon]
      --   [DDerivClause]
      -- | DTySynD Name [DTyVarBndrUnit] DType
      -- | DClassD DCxt Name [DTyVarBndrUnit] [FunDep] [DDec]
      -- | DInstanceD (Maybe Overlap)
      --   (Maybe [DTyVarBndrUnit])
      --   DCxt
      --   DType
      --   [DDec]
      -- | DForeignD DForeign
      -- | DOpenTypeFamilyD DTypeFamilyHead
      -- | DClosedTypeFamilyD DTypeFamilyHead [DTySynEqn]
      -- | DDataFamilyD Name [DTyVarBndrUnit] (Maybe DKind)
      -- | DDataInstD DataFlavor
      --   DCxt
      --   (Maybe [DTyVarBndrUnit])
      --   DType
      --   (Maybe DKind)
      --   [DCon]
      --   [DDerivClause]
      -- | DTySynInstD DTySynEqn
      -- | DRoleAnnotD Name [Role]
      -- | DStandaloneDerivD (Maybe DDerivStrategy)
      --   (Maybe [DTyVarBndrUnit])
      --   DCxt
      --   DType
      -- | DDefaultSigD Name DType
      -- | DPatSynD Name PatSynArgs DPatSynDir DPat
      -- | DPatSynSigD Name DPatSynType
      -- | DKiSigD Name DKind
      -- | DDefaultD [DType]
  
--    getStr2 (DNormalC conName _) = getStr2 conName
    --getStr2 (DRecC conName ) = getStr2 conName
    --getStr2 (DInfixD _ conName ) = getStr2 conName
    getStr2 (DForallT _  con) = getStr2 con
    getStr2 _ = "nope"
