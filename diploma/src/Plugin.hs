{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-} -- для ':
{-# LANGUAGE PolyKinds             #-} -- для "Unexpected kind variable ‘k’"
{-# LANGUAGE UndecidableInstances  #-}

module Plugin ( plugin, Set ) where

import Data.Type.Equality (type (==))
import GHC.TcPluginM.Extra (lookupModule, lookupName)
import Data.Either         (partitionEithers)
import Data.Maybe (catMaybes)
import Outputable          (Outputable, ppr, showSDocUnsafe)
import Debug.Trace         (trace)

-- GHC API
import Plugins    (Plugin (..), defaultPlugin)
import TcRnTypes  (Ct, TcPlugin (..), TcPluginResult (..), ctEvPred, ctEvidence)
import TcPluginM  (TcPluginM, tcLookupTyCon, zonkCt)
import TyCon      (TyCon, TyCon(..), tyConName, tyConFlavour, tyConBinders, isPromotedDataCon, isDataTyCon)
import Module     (mkModuleName)
import FastString (fsLit)
import OccName    (mkTcOcc)
import Type       (PredTree(..), classifyPredType, EqRel(..))
import TyCoRep    (Type (..), KindOrType)
import Var        (Var, isId, isTyVar, isTcTyVar, tcTyVarDetails, varName, DFunId)
import TcType     (pprTcTyVarDetails)
import Name       (getName)
import TcEvidence (EvTerm (..))

data TSet k

type family Set (xs :: [k]) :: TSet k

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const (Just typeLevelSetsPlugin) } 

typeLevelSetsPlugin :: TcPlugin
typeLevelSetsPlugin = 
  TcPlugin { 
             tcPluginInit  = lookupSetTyCon 
           , tcPluginSolve = solveSet
           , tcPluginStop  = const (return ())
           }


lookupSetTyCon :: TcPluginM SetDefs
lookupSetTyCon = do
    md <- lookupModule setModule setPackage
    setTyCon <- look md "Set"
    return $ SetDefs setTyCon
  where
    setModule = mkModuleName "Plugin"
    setPackage = fsLit "diploma"
    look md s = tcLookupTyCon =<< lookupName md (mkTcOcc s)


data SetDefs = SetDefs 
             {
                 set :: TyCon
             }

solveSet :: SetDefs
         -> [Ct]
         -> [Ct]
         -> [Ct]
         -> TcPluginM TcPluginResult
solveSet _ _ _ [] = return $ TcPluginOk [] []
solveSet defs givens deriveds wanteds = do
    gs <- mapM zonkCt givens
    let setConstraints = catMaybes $ fmap (getSetConstraint defs) wanteds
    let ans = fmap (\(x,y,z) -> x) setConstraints
    case setConstraints of
        [] -> return $ trace ("!!") $ TcPluginOk [] []
        _ -> do
            let xs = fmap (constraintToEvTerm defs) setConstraints
            let (lefts, rights) = partitionEithers xs
            return $ TcPluginContradiction lefts

-- здесь KindsOrTypes == [*, '[Int, Bool, Int]] -- ' здесь из вывода (те аутпута)
constraintToEvTerm :: SetDefs -> SetConstraint -> Either Ct ([(EvTerm, Ct)], [Ct]) 
constraintToEvTerm defs setConstraint = do
    let (ct, ty1, ty2) = setConstraint
    case ty1 of
        (TyConApp tyCon1 [x1, y1]) -> 
            case ty2 of
                (TyConApp tyCon2 [x2, y2]) -> trace (show $ fmap func' $ extractTypes y2) Left ct
    Left ct

extractTypes :: Type -> [Type]
extractTypes (TyConApp tyCon xs) =
    if isPromotedDataCon tyCon 
        then case xs of 
                (_ : y : xs') -> y : (concatMap extractTypes xs')
                _ -> []
        else []
extractTypes _ = []


-- data PredTree = ClassPred Class [Type]
--               | EqPred EqRel Type Type
--               | IrredPred PredType
-- data EqRel = NomEq | ReprEq deriving (Eq, Ord)
-- | A choice of equality relation. This is separate from the type 'Role'
-- because 'Phantom' does not define a (non-trivial) equality relation.

-- EqPred NomEq (ty1 :: TyConApp) (ty2 :: TyConApp)
-- TyConApp TyCon [KindOrType]
-- ty1 == TyConApp (Set :: TyCon) ([*, '[Int, Bool, Int]] :: [KindOrType]) для нашего случая
getSetConstraint :: SetDefs -> Ct -> Maybe SetConstraint
getSetConstraint defs ct = 
    case classifyPredType $ ctEvPred $ ctEvidence ct of
        (EqPred NomEq ty1 ty2) -- NomEq == nominal equality
            -> case ty1 of 
                (TyConApp tyCon1 kot1) -> case ty2 of 
                    (TyConApp tyCon2 kot2)
                        | tyConName tyCon1 == (getName $ set defs) && tyConName tyCon2 == (getName $ set defs) -> Just (ct, ty1, ty2)
                    _ -> Nothing
                _ -> Nothing
        _ -> Nothing

type SetConstraint = ( Ct    -- The constraint
                     , Type  -- Fst argument to equality constraint
                     , Type  -- Snd argument to equality constraint
                     )

-- DEBUG
-- getSetConstraint :: SetDefs -> Ct -> Maybe Ct
-- getSetConstraint defs ct = 
--     case classifyPredType $ ctEvPred $ ctEvidence ct of
--         (EqPred NomEq ty1 ty2) -- NomEq == nominal equality
--             -- |  className cls == (getName $ knownRatTyCon defs)
--             --    -> Just (ct, cls, ty)
--             -- -> trace (showSDocUnsafe $ ppr a) Nothing
--             -> case ty1 of 
--                 (TyConApp tyCon1 kot1) -> case ty2 of
--                     (TyConApp tyCon2 kot2) -> trace ("TyConApp " ++ (showSDocUnsafe $ ppr tyCon1) ++ " " ++ (showSDocUnsafe $ ppr kot1) ++ " " ++ (show $ fmap (\x -> (showSDocUnsafe $ ppr x) ++ "         " ++ func' x) kot1)) Nothing
--                     _ -> Nothing
                    
                    
--                 _ -> Nothing
--             -- -> trace (func' ty1 ++ " " ++ func' ty2) Nothing
--         _ -> Nothing

func' :: Type -> String
func' (TyVarTy var) = "TyVarTy " ++ (show $ isId var) ++ (show $ isTyVar var) ++ (show $ isTcTyVar var) ++ " "  ++ (showSDocUnsafe $ pprTcTyVarDetails $ tcTyVarDetails var) ++ " " ++ (showSDocUnsafe $ ppr var) ++ " "
func' AppTy{} = "AppTy "
func' ForAllTy{} = "ForAllTy "
func' FunTy{} = "FunTy "
func' LitTy{} = "LitTy "
func' CastTy{} = "CastTy "
func' CoercionTy{} = "CoercionTy "
func' (TyConApp tyCon kot) = "TyConApp: tyConFlavour: " ++ (tyConFlavour tyCon) ++ ", sDoc: " ++ (showSDocUnsafe $ ppr tyCon) ++ ", KindsOrTypes: " ++ (showSDocUnsafe $ ppr kot) ++ ":: " ++ (show $ fmap func' kot)
-- func' (TyConApp tyCon kot) = "TyConApp "


-- TyConApp: для '[Int, Int] 
-- 1. tyConFlavour: promoted data constructor, sDoc: ':, KindsOrTypes: [*, Int, '[Int]]
-- -> 2. tyConFlavour: built-in type, sDoc: TYPE, KindsOrTypes: ['LiftedRep]
--    -> 3. tyConFlavour: promoted data constructor, sDoc: 'LiftedRep, KindsOrTypes: []
--       -> 4. []
--    6. tyConFlavour: data type, sDoc: Int, KindsOrTypes: []
--    -> 12. []
--    7. tyConFlavour: promoted data constructor, sDoc: ':, KindsOrTypes: [*, Int, '[]]
--    -> 8. tyConFlavour: built-in type, sDoc: TYPE, KindsOrTypes: ['LiftedRep]
--       -> 9. tyConFlavour: promoted data constructor, sDoc: 'LiftedRep, KindsOrTypes: []
--          -> 10. []
--    -> 11. tyConFlavour: data type, sDoc: Int, KindsOrTypes: []
--       -> 13. []
--    -> 14. tyConFlavour: promoted data constructor, sDoc: '[], KindsOrTypes: [*]
--       -> 15. tyConFlavour: built-in type, sDoc: TYPE, KindsOrTypes: ['LiftedRep]
--          -> 16. tyConFlavour: promoted data constructor, sDoc: 'LiftedRep, KindsOrTypes: []
--             -> 17. []
