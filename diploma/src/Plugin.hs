{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Plugin ( plugin, Set ) where

import           Data.Either         (partitionEithers)
import           Data.Maybe          (catMaybes)
import           Data.Type.Equality  (type (==))
import           Data.Typeable       (typeOf, typeRepFingerprint)
import           Debug.Trace         (trace)
import           GHC.TcPluginM.Extra (evByFiat, lookupModule, lookupName)
import           Outputable          (Outputable, ppr, showSDocUnsafe)

-- GHC API
import           FastString          (fsLit)
import           Module              (mkModuleName)
import           Name                (getName)
import           OccName             (mkTcOcc)
import           Plugins             (Plugin (..), defaultPlugin)
import           TcEvidence          (EvTerm (..))
import           TcPluginM           (TcPluginM, tcLookupTyCon)
import           TcRnTypes           (Ct, TcPlugin (..), TcPluginResult (..),
                                      ctEvPred, ctEvidence)
import           TcType              (pprTcTyVarDetails)
import           TyCon               (TyCon (..), isPromotedDataCon,
                                      tyConBinders, tyConFlavour, tyConName,
                                      tyConUnique)
import           TyCoRep             (KindOrType, Type (..))
import           Type                (EqRel (..), PredTree (..),
                                      classifyPredType)
import           Var                 (isId, isTcTyVar, isTyVar, tcTyVarDetails)

data TSet k

type family Set (xs :: [k]) :: TSet k

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const (Just typeLevelSetsPlugin) }

typeLevelSetsPlugin :: TcPlugin
typeLevelSetsPlugin =
  TcPlugin { tcPluginInit  = lookupSetTyCon
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


newtype SetDefs = SetDefs { set :: TyCon }

solveSet :: SetDefs
         -> [Ct]
         -> [Ct]
         -> [Ct]
         -> TcPluginM TcPluginResult
solveSet _ _ _ [] = return $ TcPluginOk [] []
solveSet defs _ _ wanteds = do
    let setConstraints = catMaybes $ fmap (getSetConstraint defs) wanteds
    case setConstraints of
        [] -> return $ trace "!!" $ TcPluginOk [] []
        _ -> do
            let xs = fmap constraintToEvTerm setConstraints
            let (lefts, rights) = partitionEithers xs
            if not $ null lefts
                then return $ TcPluginContradiction lefts
                else do
                    let resolved = concatMap fst rights
                    let newWanteds = concatMap snd rights
                    if null resolved then return $ TcPluginOk [] [] else return $ TcPluginOk resolved newWanteds

-- здесь KindsOrTypes == [*, '[Int, Bool, Int]] -- ' здесь из вывода (те аутпута)
constraintToEvTerm :: SetConstraint -> Either Ct ([(EvTerm, Ct)], [Ct])
constraintToEvTerm setConstraint = do
    let (ct, ty1, ty2, _, _) = setConstraint
    case ty1 of
        (TyConApp _ [_, y1]) ->  -- первый _ это TyCon для типа, второй _ это *
            case ty2 of
                (TyConApp _ [_, y2]) -> do
                    let types1 = sort $ extractTypes y1
                    let types2 = sort $ extractTypes y2
                    if checkEquality types1 types2
                        then Right ([(evByFiat "set-constraint" ty1 ty2, ct)], [])
                        else Left ct
                _ -> Right ([], [ct]) -- означает, что плагин не может решить данный констрейнт (в смысле не предназначен для решения)
        _-> Right ([], [ct])

                    -- trace ((showSDocUnsafe $ ppr types1) ++ " " ++ (showSDocUnsafe $ ppr types2) ++ " " ++ (show $ fmap func' $ extractTypes y2)) Left ct
    -- Left ct

-- Здесь считаем (из аутпута трейсинга), что PromotedDataCon нам нужен тот, у которого вид KindsOrTypes
-- [*, Int, '[Int]] (так как пока что я не умею искать ': конструктор, если смотреть на Unique этого конструктора, то он 66,
-- для : конструктора должен быть таким же, можно сравнивать с ним)
extractTypes :: Type -> [Type]
extractTypes (TyConApp tyCon xs) =
    -- if isPromotedDataCon (trace (showSDocUnsafe $ ppr $ tyConUnique tyCon) tyCon)
    if isPromotedDataCon tyCon
        then case xs of
                (_ : y : xs') -> y : concatMap extractTypes xs'
                _             -> []
        else []
extractTypes _ = []

split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:xys) = (x:xs, y:ys) where (xs, ys) = split xys

sort :: [Type] -> [Type]
sort [x] = [x]
sort [] = []
sort arr = let (xs', ys') = split arr in merge (sort xs') (sort ys')
  where
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys) = do
        let xType = typeRepFingerprint $ typeOf x
        let yType = typeRepFingerprint $ typeOf y
        case compare xType yType of
            GT -> y : merge (x : xs) ys
            LT -> x : merge xs (y : ys)
            EQ -> x : merge xs ys

checkEquality :: [Type] -> [Type] -> Bool
checkEquality [] [] = True
checkEquality _ [] = False
checkEquality [] _ = False
checkEquality (x : xs) (y : ys) = do
    let xType = typeRepFingerprint $ typeOf x
    let yType = typeRepFingerprint $ typeOf y
    (xType == yType) && checkEquality xs ys


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
                        | tyConName tyCon1 == getName (set defs) && tyConName tyCon2 == getName (set defs) -> Just (ct, ty1, ty2, kot1, kot2)
                    _ -> Nothing
                _ -> Nothing
        _ -> Nothing

type SetConstraint = ( Ct    -- The Set constraint
                     , Type  -- Fst argument to equality constraint
                     , Type  -- Snd argument to equality constraint
                     , [KindOrType] -- типы для первого Set
                     , [KindOrType] -- типы для второго Set
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
