--
-- :set -XTemplateHaskell
-- import Language.Haskell.TH.Syntax
--        Language.Haskell.TH.Desugar.AST
-- replacement rules:
-- Language.Haskell.TH.Syntax. → language_Haskell_TH_Syntax_
-- Language.Haskell.TH.Desugar.AST. → language_Haskell_TH_Desugar_AST_

module Server.MetaCoq.TestMeta5 where

--import  GHC.Ba
--import Language.Haskell.TH.Syntax
--import Language.Haskell.TH

-- import Language.Haskell.TH.Syntax
-- import Language.Haskell.TH.Desugar.AST
-- import Language.Haskell.TH.Syntax
-- import Language.Haskell.TH.Desugar.Reify
-- import GHC.Maybe
-- import GHC.Types
-- import Language.Haskell.TH.Syntax

-- $(stringE . show =<< dsReify  'Language.Haskell.TH.Syntax.Name )
language_Haskell_TH_Syntax_Name = (DVarI language_Haskell_TH_Syntax_Name (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_OccName)) (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_NameFlavour)) (DConT language_Haskell_TH_Syntax_Name))) (Just language_Haskell_TH_Syntax_Name))

-- $(stringE . show =<< dsReify  'Language.Haskell.TH.Syntax.OccName )
language_Haskell_TH_Syntax_OccName = (DVarI language_Haskell_TH_Syntax_OccName (DAppT (DAppT DArrowT (DConT ghc_Base_String)) (DConT language_Haskell_TH_Syntax_OccName)) (Just language_Haskell_TH_Syntax_OccName))

-- $(stringE . show =<< dsReify  ''Language.Haskell.TH.Desugar.Reify.DsMonad)
language_Haskell_TH_Desugar_Reify_DsMonad=(DTyConI (DClassD [DAppT (DConT Language.Haskell.TH.Syntax.Quasi) (DVarT m_7566047373983751092),DAppT (DConT Control.Monad.Fail.MonadFail) (DVarT m_7566047373983751092)] language_Haskell_TH_Desugar_Reify_DsMonad [DKindedTV m_7566047373983751092 () (DAppT (DAppT DArrowT (DConT GHC.Types.Type)) (DConT GHC.Types.Type))] [] [DLetDec (DSigD language_Haskell_TH_Desugar_Reify_localDeclarations (DAppT (DVarT m_7566047373983751092) (DAppT (DConT GHC.Types.[]) (DConT Language.Haskell.TH.Syntax.Dec))))]) (Just [DInstanceD Nothing Nothing [DAppT (DConT language_Haskell_TH_Desugar_Reify_DsMonad) (DVarT m_7566047373983751099),DAppT (DConT GHC.Base.Monoid) (DVarT w_7566047373983751100)] (DAppT (DConT language_Haskell_TH_Desugar_Reify_DsMonad) (DAppT (DAppT (DConT Control.Monad.Trans.Writer.Lazy.WriterT) (DVarT w_7566047373983751100)) (DVarT m_7566047373983751099))) [],DInstanceD Nothing Nothing [DAppT (DConT language_Haskell_TH_Desugar_Reify_DsMonad) (DVarT m_7566047373983751101)] (DAppT (DConT language_Haskell_TH_Desugar_Reify_DsMonad) (DAppT (DAppT (DConT Control.Monad.Trans.State.Lazy.StateT) (DVarT s_7566047373983751102)) (DVarT m_7566047373983751101))) [],DInstanceD Nothing Nothing [DAppT (DConT language_Haskell_TH_Desugar_Reify_DsMonad) (DVarT m_7566047373983751103)] (DAppT (DConT language_Haskell_TH_Desugar_Reify_DsMonad) (DAppT (DAppT (DConT Control.Monad.Trans.Reader.ReaderT) (DVarT r_7566047373983751104)) (DVarT m_7566047373983751103))) [],DInstanceD Nothing Nothing [DAppT (DConT language_Haskell_TH_Desugar_Reify_DsMonad) (DVarT m_7566047373983751105),DAppT (DConT GHC.Base.Monoid) (DVarT w_7566047373983751106)] (DAppT (DConT language_Haskell_TH_Desugar_Reify_DsMonad) (DAppT (DAppT (DAppT (DAppT (DConT Control.Monad.Trans.RWS.Lazy.RWST) (DVarT r_7566047373983751107)) (DVarT w_7566047373983751106)) (DVarT s_7566047373983751108)) (DVarT m_7566047373983751105))) [],DInstanceD Nothing Nothing [] (DAppT (DConT language_Haskell_TH_Desugar_Reify_DsMonad) (DConT Language.Haskell.TH.Syntax.Q)) [],DInstanceD Nothing Nothing [] (DAppT (DConT language_Haskell_TH_Desugar_Reify_DsMonad) (DConT GHC.Types.IO)) [],DInstanceD Nothing Nothing [DAppT (DConT Language.Haskell.TH.Syntax.Quasi) (DVarT q_7566047373983751109),DAppT (DConT Control.Monad.Fail.MonadFail) (DVarT q_7566047373983751109)] (DAppT (DConT language_Haskell_TH_Desugar_Reify_DsMonad) (DAppT (DConT language_Haskell_TH_Desugar_Reify_DsM) (DVarT q_7566047373983751109))) []])


data Term = Skip
  | DsMonad
  | GHC_Types_Type
  | GHC_Maybe
  | DPatSynI 
  | DAppT Term Term
  | DArrowT
  | DConT Term
  | DConstrainedT [Term] Term 
  | DForallInvis [Term]
  | DForallT Term Term
  | DKindedTV Term Term Term
-- TH: data TyVarBndr flag = PlainTV  Name flag      -- ^ @a@
--                    | KindedTV Name flag Kind -- ^ @(a :: k)@

  | DTyConI Term (Maybe [Term]) Term
  --DVarI a b c d :: a -> b -> Maybe c -> d
  | DVarI Term Term (Maybe Term)
  | DVarT Term
  | DTySynD Term  [Term] Term 
  | InferredSpec
--  | String String

--ghci> $(stringE . show =<< dsReify  'f1 )
f1= Skip --dsReify language_Haskell_TH_Desugar_AST_DPatSynI


q_6989586621680124703 = mkName "temp"

-- data Specificity = SpecifiedSpec          -- ^ @a@
--                  | InferredSpec           -- ^ @{a}@
-- /mnt/data1/2023/07/05/ghc/libraries/template-haskell/Language/Haskell/TH/Syntax.hs
nexttest2 :: Term
nexttest2= DVarI f1 (DForallT
                     (DForallInvis [
                         DKindedTV q_6989586621680124703 InferredSpec
                         (DAppT (DAppT DArrowT (DConT GHC_Types_Type)) (DConT GHC_Types_Type))
                                   ])
                     
                     (DConstrainedT [DAppT (DConT DsMonad) (DVarT q_6989586621680124703)]
                      (DAppT (DVarT q_6989586621680124703)
                       (DAppT
                        (DConT GHC_Maybe)
                        (DConT language_Haskell_TH_Desugar_AST_DInfo)
                       )
                      ) -- dappt
                     ) -- constrainedt
                    ) -- forall
           Nothing

language_Haskell_TH_Desugar_AST_DPatSynI = DVarI language_Haskell_TH_Desugar_AST_DPatSynI (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_Name)) (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Desugar_AST_DPatSynType)) (DConT language_Haskell_TH_Desugar_AST_DInfo))) (language_Haskell_TH_Desugar_AST_DInfo)

next_test = DVarI
  -- Name -> DType -> Maybe Name -> DInfo’ has only three
  (language_Haskell_TH_Desugar_AST_DPatSynI (
      DAppT
        (DAppT DArrowT
          (DConT language_Haskell_TH_Syntax_Name))

        -- appt
        (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Desugar_AST_DPatSynType)
               )
          
          (DConT language_Haskell_TH_Desugar_AST_DInfo))
        
      )
    (language_Haskell_TH_Desugar_AST_DInfo)
    
  )

--toBangArray :: DInfo -> [BangType]
--toBangArray _ = mempty

b = NormalC toName (
  language_Haskell_TH_Desugar_AST_DPatSynI
    
    toBangArray [
      (Bang NoSourceUnpackedness NoSourceStrictness,
       ConT language_Haskell_TH_Syntax_Name
      ),
                   
        (Bang NoSourceUnpackedness NoSourceStrictness,
          ConT language_Haskell_TH_Desugar_AST_DPatSynType)
      ]
  )
    
             
-- reification of
substract = [
  TyConI (
      DataD [] language_Haskell_TH_Desugar_AST_DInfo [] Nothing
      [
             NormalC language_Haskell_TH_Desugar_AST_DTyConI [
          
          (
            Bang NoSourceUnpackedness NoSourceStrictness,
            ConT language_Haskell_TH_Desugar_AST_DDec),

            (Bang NoSourceUnpackedness NoSourceStrictness,AppT (
                ConT GHC_Maybe) (AppT ListT (ConT
                                                                                                     language_Haskell_TH_Desugar_AST_DInstanceDec)))
            
          ],

             NormalC language_Haskell_TH_Desugar_AST_DVarI [(Bang NoSourceUnpackedness NoSourceStrictness,
                                                             ConT language_Haskell_TH_Syntax_Name),
                                                            (Bang NoSourceUnpackedness NoSourceStrictness,
                                                             ConT language_Haskell_TH_Desugar_AST_DType),(Bang NoSourceUnpackedness NoSourceStrictness,AppT (
                                                                                                               ConT GHC_Maybe)
                                                                                                                                                          (ConT language_Haskell_TH_Syntax_Name))],
             NormalC
             language_Haskell_TH_Desugar_AST_DTyVarI [(Bang NoSourceUnpackedness NoSourceStrictness,
                                                       ConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,
                                                                                                ConT language_Haskell_TH_Desugar_AST_DKind)],NormalC language_Haskell_TH_Desugar_AST_DPrimTyConI [(Bang NoSourceUnpackedness NoSourceStrictness,
                                                                                                                                                                                                     ConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,
                                                                                                                                                                                                                                              ConT GHC_Types_Int),(Bang NoSourceUnpackedness NoSourceStrictness,
                                                                                                                                                                                                                                                                                                                                                                                                                                                         ConT GHC_Types_Bool)
                                                                                                                                                                                                   ]
      ,

      b
             
      ]-- dataD
      [])
  ] -- substrate



-- $(stringE _ show =<< dsReify GHC_Types_Type)
--DTyConI :: DDec -> Maybe [DInstanceDec] -> DInfo

atype = DTyConI (DTySynD GHC_Types_Type [] (DConT GHC_Types_Type)) Nothing

-- $(stringE _ show =<< dsReifyType 'DTyConI)
dtyconi  = DAppT (DAppT DArrowT (DConT language_Haskell_TH_Desugar_AST_DDec)) (DAppT (DAppT DArrowT (DAppT (DConT GHC_Maybe) (DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DInstanceDec)))) (DConT just language_Haskell_TH_Desugar_AST_DInfo))

language_Haskell_TH_Desugar_AST_DInfo = dinfo

dinfo = DAppT (DAppT DArrowT (DConT language_Haskell_TH_Desugar_AST_DType)) (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Desugar_AST_DType)) (DConT language_Haskell_TH_Desugar_AST_DType))

-- stringE _ show =<< dsReifyType 'DArrowT
dtype = DConT language_Haskell_TH_Desugar_AST_DType

-- $(stringE _ show =<< dsReifyType 'DConT)
dcont = DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_Name)) (DConT language_Haskell_TH_Desugar_AST_DType)

-- $(stringE _ show =<< dsReifyType 'language_Haskell_TH_Syntax_Name)
name = DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_OccName)) (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_NameFlavour)) (DConT language_Haskell_TH_Syntax_Name))

occname = DAppT (DAppT DArrowT (DConT GHC_Base_String)) (DConT language_Haskell_TH_Syntax_OccName)

-- $(stringE _ show =<< dsReify language_Haskell_TH_Syntax_NameFlavour)
name_flavor = DTyConI (DDataD Data [] language_Haskell_TH_Syntax_NameFlavour [] Nothing [DCon [] [] language_Haskell_TH_Syntax_NameS (DNormalC False []) (DConT language_Haskell_TH_Syntax_NameFlavour),DCon [] [] language_Haskell_TH_Syntax_NameQ (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_ModName)]) (DConT language_Haskell_TH_Syntax_NameFlavour),DCon [] [] language_Haskell_TH_Syntax_NameU (DNormalC False [(Bang NoSourceUnpackedness SourceStrict,DConT language_Haskell_TH_Syntax_Uniq)]) (DConT language_Haskell_TH_Syntax_NameFlavour),DCon [] [] language_Haskell_TH_Syntax_NameL (DNormalC False [(Bang NoSourceUnpackedness SourceStrict,DConT language_Haskell_TH_Syntax_Uniq)]) (DConT language_Haskell_TH_Syntax_NameFlavour),DCon [] [] language_Haskell_TH_Syntax_NameG (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_NameSpace),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_PkgName),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_ModName)]) (DConT language_Haskell_TH_Syntax_NameFlavour)] []) Nothing

-- :type DTyConI
-- DTyConI :: DDec -> Maybe [DInstanceDec] -> DInfo

-- ghci> :info DDec
-- type DDec :: *
-- data DDec
--   = DLetDec DLetDec
--   | DDataD DataFlavor
--            DCxt
--            Name
--            [DTyVarBndrUnit]
--            (Maybe DKind)
--            [DCon]
--            [DDerivClause]
--   | DTySynD Name [DTyVarBndrUnit] DType
--   | DClassD DCxt Name [DTyVarBndrUnit] [FunDep] [DDec]
--   | DInstanceD (Maybe Overlap)
--                (Maybe [DTyVarBndrUnit])
--                DCxt
--                DType
--                [DDec]
--   | DForeignD DForeign
--   | DOpenTypeFamilyD DTypeFamilyHead
--   | DClosedTypeFamilyD DTypeFamilyHead [DTySynEqn]
--   | DDataFamilyD Name [DTyVarBndrUnit] (Maybe DKind)
--   | DDataInstD DataFlavor
--                DCxt
--                (Maybe [DTyVarBndrUnit])
--                DType
--                (Maybe DKind)
--                [DCon]
--                [DDerivClause]
--   | DTySynInstD DTySynEqn
--   | DRoleAnnotD Name [Role]
--   | DStandaloneDerivD (Maybe DDerivStrategy)
--                       (Maybe [DTyVarBndrUnit])
--                       DCxt
--                       DType
--   | DDefaultSigD Name DType
--   | DPatSynD Name PatSynArgs DPatSynDir DPat
--   | DPatSynSigD Name DPatSynType
--   | DKiSigD Name DKind
--   | DDefaultD [DType]
--   	-- Defined at language/Haskell/TH/Desugar/AST_hs:111:1
-- instance Eq DDec
--   -- Defined at language/Haskell/TH/Desugar/AST_hs:158:21
-- instance Show DDec
--   -- Defined at language/Haskell/TH/Desugar/AST_hs:158:25
-- instance Lift DDec
--   -- Defined at language/Haskell/TH/Desugar/AST_hs:158:46
-- instance Desugar [Dec] [DDec]
--   -- Defined at language/Haskell/TH/Desugar_hs:197:10

-- DDec

ddec = (DTyConI (DDataD Data [] language_Haskell_TH_Desugar_AST_DDec [] Nothing [DCon [] [] language_Haskell_TH_Desugar_AST_DLetDec (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DLetDec)]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DDataD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_Util_DataFlavor),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DCxt),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DTyVarBndrUnit)),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Maybe) (DConT language_Haskell_TH_Desugar_AST_DKind)),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DCon)),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DDerivClause))]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DTySynD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DTyVarBndrUnit)),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DType)]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DClassD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DCxt),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DTyVarBndrUnit)),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Syntax_FunDep)),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DDec))]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DInstanceD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Maybe) (DConT language_Haskell_TH_Syntax_Overlap)),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Maybe) (DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DTyVarBndrUnit))),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DCxt),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DType),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DDec))]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DForeignD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DForeign)]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DOpenTypeFamilyD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DTypeFamilyHead)]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DClosedTypeFamilyD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DTypeFamilyHead),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DTySynEqn))]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DDataFamilyD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DTyVarBndrUnit)),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Maybe) (DConT language_Haskell_TH_Desugar_AST_DKind))]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DDataInstD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_Util_DataFlavor),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DCxt),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Maybe) (DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DTyVarBndrUnit))),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DType),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Maybe) (DConT language_Haskell_TH_Desugar_AST_DKind)),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DCon)),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DDerivClause))]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DTySynInstD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DTySynEqn)]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DRoleAnnotD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Syntax_Role))]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DStandaloneDerivD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Maybe) (DConT language_Haskell_TH_Desugar_AST_DDerivStrategy)),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Maybe) (DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DTyVarBndrUnit))),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DCxt),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DType)]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DDefaultSigD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DType)]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DPatSynD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_PatSynArgs),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DPatSynDir),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DPat)]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DPatSynSigD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DPatSynType)]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DKiSigD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DKind)]) (DConT language_Haskell_TH_Desugar_AST_DDec),DCon [] [] language_Haskell_TH_Desugar_AST_DDefaultD (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DType))]) (DConT language_Haskell_TH_Desugar_AST_DDec)] []) Nothing)

-- $(stringE _ show =<< dsReify DInstanceDec)
dinstanced = (DTyConI (DTySynD language_Haskell_TH_Desugar_AST_DInstanceDec [] (DConT language_Haskell_TH_Desugar_AST_DDec)) Nothing)

-- $(stringE _ show =<< dsReify 'DTySynD)
dtysynd = (DVarI language_Haskell_TH_Desugar_AST_DTySynD (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_Name)) (DAppT (DAppT DArrowT (DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DTyVarBndrUnit))) (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Desugar_AST_DType)) (DConT language_Haskell_TH_Desugar_AST_DDec)))) (language_Haskell_TH_Desugar_AST_DDec))


--language_Haskell_TH_Desugar_AST_DVarI
--  :: Name -> DType -> Maybe Name -> DInfo
-- $(stringE _ show =<< dsReify 'DVarI)
dvari = (DVarI language_Haskell_TH_Desugar_AST_DVarI (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_Name)) (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Desugar_AST_DType)) (DAppT (DAppT DArrowT (DAppT (DConT GHC_Maybe) (DConT language_Haskell_TH_Syntax_Name))) (DConT language_Haskell_TH_Desugar_AST_DInfo)))) (language_Haskell_TH_Desugar_AST_DInfo))

-- $(stringE _ show =<< dsReify language_Haskell_TH_Desugar_AST_DInfo)
adinfo  = (DTyConI (DDataD Data [] language_Haskell_TH_Desugar_AST_DInfo [] Nothing [DCon [] [] language_Haskell_TH_Desugar_AST_DTyConI(DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DDec),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Maybe) (DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DInstanceDec)))]) (DConT language_Haskell_TH_Desugar_AST_DInfo),DCon [] [] language_Haskell_TH_Desugar_AST_DVarI (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DType),(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Maybe) (DConT language_Haskell_TH_Syntax_Name))]) (DConT language_Haskell_TH_Desugar_AST_DInfo),DCon [] [] language_Haskell_TH_Desugar_AST_DTyVarI (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DKind)]) (DConT language_Haskell_TH_Desugar_AST_DInfo),DCon [] [] language_Haskell_TH_Desugar_AST_DPrimTyConI (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DConT GHC_Types_Int),(Bang NoSourceUnpackedness NoSourceStrictness,DConT GHC_Types_Bool)]) (DConT language_Haskell_TH_Desugar_AST_DInfo),DCon [] [] language_Haskell_TH_Desugar_AST_DPatSynI (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Desugar_AST_DPatSynType)]) (DConT language_Haskell_TH_Desugar_AST_DInfo)] []) Nothing)

-- $(stringE _ show =<< dsReify 'DNormalC)
normalc = (DVarI language_Haskell_TH_Desugar_AST_DNormalC (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Desugar_AST_DDeclaredInfix)) (DAppT (DAppT DArrowT (DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Desugar_AST_DBangType))) (DConT language_Haskell_TH_Desugar_AST_DConFields))) (language_Haskell_TH_Desugar_AST_DConFields))

-- $(stringE _ show =<< dsReify language_Haskell_TH_Desugar_AST_DDeclaredInfix)

declared_infix = (DTyConI (DTySynD language_Haskell_TH_Desugar_AST_DDeclaredInfix [] (DConT GHC_Types_Bool)) Nothing)


-- $(stringE _ show =<< dsReify GHC_Types_Bool)

ghc_bool = (DTyConI (DDataD Data [] GHC_Types_Bool [] Nothing [DCon [] [] GHC_Types_False (DNormalC False []) (DConT GHC_Types_Bool),DCon [] [] GHC_Types_True (DNormalC False []) (DConT GHC_Types_Bool)] []) Nothing)


-- $(stringE _ show =<< dsReify 'DataD)
datad = (DVarI language_Haskell_TH_Syntax_DataD (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_Cxt)) (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_Name)) (DAppT (DAppT DArrowT (DAppT (DConT GHC_Types_empty_array) (DAppT (DConT language_Haskell_TH_Syntax_TyVarBndr) (DConT GHC_Tuple_())))) (DAppT (DAppT DArrowT (DAppT (DConT GHC_Maybe) (DConT language_Haskell_TH_Syntax_Kind))) (DAppT (DAppT DArrowT (DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Syntax_Con))) (DAppT (DAppT DArrowT (DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Syntax_DerivClause))) (DConT language_Haskell_TH_Syntax_Dec))))))) (language_Haskell_TH_Syntax_Dec))

-- $(stringE _ show =<< dsReify language_Haskell_TH_Syntax_Cxt)
ctx = (DTyConI (DTySynD language_Haskell_TH_Syntax_Cxt [] (DAppT (DConT GHC_Types_empty_array) (DConT language_Haskell_TH_Syntax_Pred))) Nothing)

--type Pred :: *
--type Pred = language_Haskell_TH_Syntax_Type

th_pred= (DTyConI (DTySynD language_Haskell_TH_Syntax_Pred [] (DConT language_Haskell_TH_Syntax_Type)) Nothing)

---
--ghci> $(stringE _ show =<< dsReify language_Haskell_TH_Syntax_Type)
th_syntax_type = (DTyConI (DDataD Data [] language_Haskell_TH_Syntax_Type [] Nothing [DCon [] [] language_Haskell_TH_Syntax_ForallT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DAppT (DConT language_Haskell_TH_Syntax_TyVarBndr) (DConT language_Haskell_TH_Syntax_Specificity))),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Cxt),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_ForallVisT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DAppT (DConT GHC_Types_empty_array) (DAppT (DConT language_Haskell_TH_Syntax_TyVarBndr) (DConT GHC_Tuple_()))),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_AppT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_AppKindT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Kind)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_SigT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Kind)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_VarT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_ConT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_PromotedT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_InfixT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_UInfixT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Name),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_ParensT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_TupleT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT GHC_Types_Int)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_UnboxedTupleT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT GHC_Types_Int)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_UnboxedSumT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_SumArity)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_ArrowT (DNormalC False []) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_MulArrowT (DNormalC False []) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_EqualityT (DNormalC False []) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_ListT (DNormalC False []) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_PromotedTupleT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT GHC_Types_Int)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_PromotedNilT (DNormalC False []) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_PromotedConsT (DNormalC False []) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_StarT (DNormalC False []) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_ConstraintT (DNormalC False []) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_LitT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_TyLit)]) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_WildCardT (DNormalC False []) (DConT language_Haskell_TH_Syntax_Type),DCon [] [] language_Haskell_TH_Syntax_ImplicitParamT (DNormalC False [(Bang NoSourceUnpackedness NoSourceStrictness,DConT GHC_Base_String),(Bang NoSourceUnpackedness NoSourceStrictness,DConT language_Haskell_TH_Syntax_Type)]) (DConT language_Haskell_TH_Syntax_Type)] []) Nothing)


-- $(stringE _ show =<< dsReify 'language_Haskell_TH_Syntax_ForallT )
th_forall_t = (DVarI language_Haskell_TH_Syntax_ForallT (DAppT (DAppT DArrowT (DAppT (DConT GHC_Types_empty_array) (DAppT (DConT language_Haskell_TH_Syntax_TyVarBndr) (DConT language_Haskell_TH_Syntax_Specificity)))) (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_Cxt)) (DAppT (DAppT DArrowT (DConT language_Haskell_TH_Syntax_Type)) (DConT language_Haskell_TH_Syntax_Type)))) (Language_Haskell_TH_Syntax_Type))

mkName :: String -> Term
mkName a = String a


dsReify :: String -> Term
dsReify a = DArrowT 


