--{-# LANGUAGE TemplateHaskell, UnboxedTuples, ParallelListComp, CPP #-}

module Server.MetaCoq.TestMeta5 where
import System.Posix.Internals (CTermios)
-- import Language.Haskell.TH.Syntax
-- import Language.Haskell.TH.Desugar.AST
-- import Language.Haskell.TH.Syntax
-- import Language.Haskell.TH.Desugar.Reify
-- import GHC.Maybe
-- import GHC.Types
-- import Language.Haskell.TH.Syntax

mkName :: String -> Term
mkName a = String a

--toName :: DInfo -> Name
toName = mkName "a"

dsReify :: String -> Term
dsReify a = DArrowT 
--DVarI a b c d :: a -> b -> Maybe c -> d

data Term = 
  DVarI Term Term (Maybe Term)
  | DForallT Term Term
  | DConstrainedT [Term] Term 
  | InferredSpec
  | DForallInvis [Term]
  | DKindedTV (Maybe Term) Term Term
  | DAppT Term Term
  | DArrowT
  | DConT String
  | String String
  | DVarT (Maybe Term)


--ghci> $(stringE . show =<< dsReify  'f1 )
f1= dsReify "Language.Haskell.TH.Desugar.AST.DPatSynI"


q_6989586621680124703 = Just (mkName "temp")

nexttest2 :: Maybe Term
nexttest2= Just (DVarI f1 (DForallT (DForallInvis [DKindedTV q_6989586621680124703 InferredSpec (DAppT (DAppT DArrowT (DConT "GHC.Types.Type")) (DConT "GHC.Types.Type"))]) (DConstrainedT [DAppT (DConT "Language.Haskell.TH.Desugar.Reify.DsMonad") (DVarT q_6989586621680124703)] (DAppT (DVarT q_6989586621680124703) (DAppT (DConT "GHC.Maybe.Maybe") (DConT "Language.Haskell.TH.Desugar.AST.DInfo"))))) Nothing)

-- next_test = Just (
--   DVarI
--   -- Name -> DType -> Maybe Name -> DInfo’ has only three
--   (Language.Haskell.TH.Desugar.AST.DPatSynI (
--       DAppT
--         (DAppT DArrowT
--           (DConT ''Language.Haskell.TH.Syntax.Name))

--         -- appt
--         (DAppT (DAppT DArrowT (DConT ''Language.Haskell.TH.Desugar.AST.DPatSynType)
--                )
          
--           (DConT ''Language.Haskell.TH.Desugar.AST.DInfo))
        
--       )
--     (Just ''Language.Haskell.TH.Desugar.AST.DInfo)
    
--   )

--toBangArray :: DInfo -> [BangType]
--toBangArray _ = mempty

-- b = NormalC toName (
--   Language.Haskell.TH.Desugar.AST.DPatSynI
    
--     toBangArray [
--       (Bang NoSourceUnpackedness NoSourceStrictness,
--        ConT ''Language.Haskell.TH.Syntax.Name
--       ),
                   
--         (Bang NoSourceUnpackedness NoSourceStrictness,
--           ConT ''Language.Haskell.TH.Desugar.AST.DPatSynType)
--       ]
--   )
    
             
-- -- reification of
-- substract = [
--   TyConI (
--       DataD [] ''Language.Haskell.TH.Desugar.AST.DInfo [] Nothing
--       [
--              NormalC Language.Haskell.TH.Desugar.AST.DTyConI [
          
--           (
--             Bang NoSourceUnpackedness NoSourceStrictness,
--             ConT ''Language.Haskell.TH.Desugar.AST.DDec),

--             (Bang NoSourceUnpackedness NoSourceStrictness,AppT (
--                 ConT ''GHC.Maybe.Maybe) (AppT ListT (ConT
--                                                                                                      ''Language.Haskell.TH.Desugar.AST.DInstanceDec)))
            
--           ],

--              NormalC Language.Haskell.TH.Desugar.AST.DVarI [(Bang NoSourceUnpackedness NoSourceStrictness,
--                                                              ConT ''Language.Haskell.TH.Syntax.Name),
--                                                             (Bang NoSourceUnpackedness NoSourceStrictness,
--                                                              ConT ''Language.Haskell.TH.Desugar.AST.DType),(Bang NoSourceUnpackedness NoSourceStrictness,AppT (
--                                                                                                                ConT ''GHC.Maybe.Maybe)
--                                                                                                                                                           (ConT ''Language.Haskell.TH.Syntax.Name))],
--              NormalC
--              Language.Haskell.TH.Desugar.AST.DTyVarI [(Bang NoSourceUnpackedness NoSourceStrictness,
--                                                        ConT ''Language.Haskell.TH.Syntax.Name),(Bang NoSourceUnpackedness NoSourceStrictness,
--                                                                                                 ConT ''Language.Haskell.TH.Desugar.AST.DKind)],NormalC Language.Haskell.TH.Desugar.AST.DPrimTyConI [(Bang NoSourceUnpackedness NoSourceStrictness,
--                                                                                                                                                                                                      ConT ''Language.Haskell.TH.Syntax.Name),(Bang NoSourceUnpackedness NoSourceStrictness,
--                                                                                                                                                                                                                                               ConT ''GHC.Types.Int),(Bang NoSourceUnpackedness NoSourceStrictness,
--                                                                                                                                                                                                                                                                                                                                                                                                                                                          ConT ''GHC.Types.Bool)
--                                                                                                                                                                                                    ]
--       ,

--       b
             
--       ]-- dataD
--       [])
--   ] -- substrate


