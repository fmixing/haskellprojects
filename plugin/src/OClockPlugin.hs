{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module OClockPlugin (plugin) where

-- external
import Data.Maybe          (catMaybes)
import Data.Either         (partitionEithers)
import GHC.TcPluginM.Extra (lookupModule, lookupName, tracePlugin, evByFiat)
import Debug.Trace         (trace)
import Data.Typeable (typeOf)
import Outputable (Outputable, ppr, showSDocUnsafe)

-- GHC API
import Class      (className, Class(..))
import Type       (PredTree(..), classifyPredType)
import Name       (Name(..), getName, nameUnique)
import Var        (Var, isId, isTyVar, isTcTyVar, tcTyVarDetails, varName)
import Unique     (getKey)
import FastString (fsLit)
import Module     (mkModuleName)
import OccName    (mkTcOcc) -- OccName represents names as strings with just a little more information: the "namespace" 
                            -- that the name came from, e.g. the namespace of value, type constructors or data constructors
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm)
import TcType     (pprTcTyVarDetails)
import TcPluginM  (TcPluginM, tcLookupTyCon, zonkCt)
import TcRnTypes  (Ct, TcPlugin (..), TcPluginResult (..), isWanted, ctEvPred, ctEvidence, ctEvTerm)
import TyCon      (TyCon, TyCon(..))
import TyCoRep    (Type (..), debugPprType)

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const (Just oclockPlugin) } -- const means skip command line options


-- type TcPluginSolver = [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult

oclockPlugin :: TcPlugin
oclockPlugin = 
  TcPlugin { 
         --  tcPluginInit  :: TcPluginM s
             tcPluginInit  = lookupRatTyCon -- Initialize plugin, when entering type-checker.
                                            -- [This allows the plugin to look things up in the context, initialise mutable state 
                                            -- or open a connection to an external process (e.g. an external SMT solver). 
                                            -- The plugin can return a result of any type it likes, and the result will be 
                                            -- passed to the other two fields.
                                            -- One can also perform IO operations in tcPluginInit, and have the state contain IORefs.]

        --   tcPluginSolve :: s -> TcPluginSolver
           , tcPluginSolve = solveDivRat -- a solver function which performs calculation 
                                         -- [This function is provided with the current set of constraints, 
                                         -- and should return a TcPluginResult that indicates whether a contradiction was found
                                         -- or progress was made.]

         --  tcPluginStop  :: s -> TcPluginM ()
           , tcPluginStop  = const (return ()) -- cleaning up the state when exiting the type-checker
                                               -- [allowing the plugin to dispose of any resources it has allocated 
                                               -- (e.g. terminating the SMT solver process)]
           }

-- TcPluginM - [Plugin code runs in the TcPluginM monad, which provides a restricted interface to GHC API functionality
-- that is relevant for typechecker plugins, including IO and reading the environment.] Narrowing of TcM
-- TyCon - type constructor: [An abstract representation of a type constructor. TyCon objects can be built using mkTyCon.]
--      1) Data declarations: data Foo = ... creates the Foo type constructor of kind *
--      2) Type synonyms: type Foo = ... creates the Foo type constructor
--      3) Newtypes: newtype Foo a = MkFoo ... creates the Foo type constructor of kind * -> *
--      4) Class declarations: class Foo where creates the Foo type constructor of kind *
-- TcM - type-checking and renaming monad
lookupRatTyCon :: TcPluginM RatDefs
lookupRatTyCon = do
    md <- lookupModule ratModule gcdPackage
    rat <- look md "Rat"
    divRat <- look md "DivRat"
    knownRat <- look md "KnownRat"
    return $ RatDefs rat divRat knownRat
  where
    ratModule = mkModuleName "Time.Rational"
    gcdPackage = fsLit "o-clock"
    look md s = tcLookupTyCon =<< lookupName md (mkTcOcc s) -- tcLookupTyCon :: Name -> TcM TyCon

data RatDefs = RatDefs
     { ratTyCon :: TyCon -- data Rat = Nat ::% Nat     
     , divRatTyCon :: TyCon -- type family DivRat (m :: Rat) (n :: Rat) :: Rat
     , knownRatTyCon :: TyCon -- class KnownRat (r :: Rat)
     }


-- [The plugin should
-- return TcPluginContradiction with a list of impossible constraints (which must be a subset of those passed in), 
--   so they can be turned into errors; or
-- return TcPluginOk with lists of solved and new constraints (the former must be a subset of those passed in 
--   and must be supplied with corresponding evidence terms).]

-- data TcPluginResult =
--      TcPluginContradiction [Ct] where [Ct] is a list of wanted constraints that are considered insolvable
--      | TcPluginOk [(EvTerm, Ct)] [Ct] where [(EvTerm, Ct)] is a list of solved constraints (EvTerm is an evidence),
--        [Ct] is a list of new wanted constraints
--        e.g. x + GCD 6 8 ~ 2 + x -> TcPluginOK [("Believe me", x + GCD 6 8 ~ 2 + x)] [GCD 6 8 ~ 2]

-- givens Ct output: [G] $dKnownRat_a1Bd {0}:: KnownRat a (CDictCan) [G] means given
-- wanteds Ct output: [WD] $dKnownRat_a1Bh {0}:: KnownRat a0 (CDictCan) [WD] means Wanted Deriv
solveDivRat :: RatDefs 
            -> [Ct] -- given constraints
            -> [Ct] -- derived constraints
            -> [Ct] -- wanted constraints
            -> TcPluginM TcPluginResult
solveDivRat _ _ _ [] = return $ TcPluginOk [] []
solveDivRat defs givens deriveds wanteds = do -- needed wanteds are of the CDictCan type
    gs <- mapM zonkCt givens
    let krConstraints = catMaybes $ fmap (getKnownRatConstraint defs) wanteds -- filter all the KnownRat constraints    
    -- let krConstraints = catMaybes $ fmap (\x -> getKnownRatConstraint defs $ (trace ((showSDocUnsafe $ ppr gs) ++ "?? " ++ (showSDocUnsafe $ ppr x)) x)) wanteds -- filter all the KnownRat constraints
    case krConstraints of
        [] -> return $ TcPluginOk [] []
        _ -> do
            let xs = fmap (constraintToEvTerm defs gs) krConstraints
            let (lefts, rights) = partitionEithers xs
            if not $ null lefts 
                then return $ (TcPluginContradiction lefts)
                else do 
                    let resolved = concatMap fst rights
                    let newWanteds = concatMap snd rights
                    if null resolved then return $ TcPluginOk [] [] else return $ TcPluginOk resolved newWanteds
                    

constraintToEvTerm :: RatDefs -> [Ct] -> KrConstraint -> Either Ct ([(EvTerm, Ct)], [Ct])
constraintToEvTerm defs givensStart krConstraint = do
    let (ct, cls, ty) = krConstraint
    let gs = catMaybes $ fmap (getKnownRatConstraint defs) givensStart
    case ty of
        TyConApp tyCon kindsOrTypes -> 
            if (getName tyCon) == (getName $ divRatTyCon defs)
            then do
                let bothKnownRat = or $ fmap (checkKnownRatConstraint defs gs) kindsOrTypes
                -- if both are KnownRat then there should be made EvTerm for them, and returned (Right ((EvTerm, ct), []))
                -- else there should be returned (Left ct)
                if bothKnownRat then Right ([(evByFiat "ghc-typelits-gcd" undefined undefined, ct)], []) else Left ct
            else Right ([], [ct])
        _ -> Right ([], [ct]) -- there should be returned Right ([], [ct])

checkKnownRatConstraint :: RatDefs -> [KrConstraint] -> Type -> Bool 
checkKnownRatConstraint defs givensStart ty = case ty of
        (TyVarTy var) -> checkIsElem (varName var) givensStart
        _ -> error $ "strange type of DivRat parameter " ++ (showSDocUnsafe $ ppr ty) ++ ": " ++ (func' ty)
    
checkIsElem :: Name -> [KrConstraint] -> Bool
checkIsElem name [] = False
checkIsElem name (krConstraint : krConstraints) = do
    let (ct, cls, ty) = krConstraint
    case ty of
        (TyVarTy var) -> getNameUniqueId name == getNameUniqueId (varName var)
        _ -> checkIsElem name krConstraints

getNameUniqueId :: Name -> Int
getNameUniqueId name = getKey $ nameUnique name


-- data PredTree = ClassPred Class [Type]   -- Class predicates e.g. (Num a)
--               | EqPred Type Type         -- Equality predicates e.g. (a ~ b)
--               | TuplePred [PredType]     -- Tuples of predicates e.g. (Num a, a~b)
--               | IrredPred PredType       -- Higher order predicates e.g. (c a)

-- classifyPredType :: PredType -> PredTree
-- ctEvPred :: CtEvidence -> TcPredType
-- ctEvTerm :: CtEvidence -> EvTerm
-- type TcPredType = PredType
-- type PredType = Type

-- ty (DivRat a b) has TyConApp constructor
-- getKnownRatConstraint :: RatDefs -> Ct -> Maybe KrConstraint
-- getKnownRatConstraint defs ct =
--     case classifyPredType $ ctEvPred $ ctEvidence ct of
--         ClassPred cls [ty]
--             |  className cls == (getName $ knownRatTyCon defs) -- KnownRat bc we need to check if KnownRat (Div a b) 
--                -> Just (ct, cls, ty)
--         _ -> Nothing

type KrConstraint = ( Ct    -- The constraint
                    , Class -- KnownRat class
                    , Type  -- The argument to KnownRat
                    )

-- Debug fuction
getKnownRatConstraint :: RatDefs -> Ct -> Maybe KrConstraint
getKnownRatConstraint defs ct =
    case classifyPredType $ ctEvPred $ ctEvidence ct of
        ClassPred cls xs -> case trace ((show (fmap (\x -> show $ typeOf x) xs)) ++ "!!" ++ (show (fmap (\x -> showSDocUnsafe $ ppr x) xs)) ++ "!!" ++ (show (fmap (\x -> showSDocUnsafe $ debugPprType x) xs))) xs of
                                [] -> Nothing
                                [ty] -> case ty of -- FalseTrueTrue for var means that the data constructor is TcTyVar, Used only during type inference
                                    (TyVarTy var) -> func ty cls ct defs ("TyVarTy " ++ (show $ isId var) ++ (show $ isTyVar var) ++ (show $ isTcTyVar var) ++ " "  ++ (showSDocUnsafe $ pprTcTyVarDetails $ tcTyVarDetails var) ++ " &&"  ++ (showSDocUnsafe $ ppr $ ctEvPred $ ctEvidence ct) ++ " %%" ++ (showSDocUnsafe $ ppr $ ctEvTerm $ ctEvidence ct) ++ " " ++ (showSDocUnsafe $ ppr var) ++ " ")
                                    AppTy{} -> func ty cls ct defs "AppTy "
                                    ForAllTy{} -> func ty cls ct defs "ForAllTy "
                                    FunTy{} -> func ty cls ct defs "FunTy "
                                    LitTy{} -> func ty cls ct defs "LitTy "
                                    CastTy{} -> func ty cls ct defs "CastTy "
                                    CoercionTy{} -> func ty cls ct defs "CoercionTy "
                                    (TyConApp tyCon kot) -> func ty cls ct defs ("TyConApp " ++ (showSDocUnsafe $ ppr tyCon) ++ " " ++ (showSDocUnsafe $ ppr kot) ++ " " ++ (show $ fmap (\x -> func' x) kot))
                                _ -> Nothing
        _ -> Nothing
-- Debug fuction
func :: Type -> Class -> Ct -> RatDefs -> String -> Maybe KrConstraint
func ty cls ct defs s = 
    if className cls == (trace (s ++ (showSDocUnsafe $ ppr (getName $ knownRatTyCon defs))) (getName $ knownRatTyCon defs))
    then Just (ct, cls, ty)
    else Nothing

func' :: Type -> String
func' (TyVarTy var) = "TyVarTy " ++ (show $ isId var) ++ (show $ isTyVar var) ++ (show $ isTcTyVar var) ++ " "  ++ (showSDocUnsafe $ pprTcTyVarDetails $ tcTyVarDetails var) ++ " " ++ (showSDocUnsafe $ ppr var) ++ " "
func' AppTy{} = "AppTy "
func' ForAllTy{} = "ForAllTy "
func' FunTy{} = "FunTy "
func' LitTy{} = "LitTy "
func' CastTy{} = "CastTy "
func' CoercionTy{} = "CoercionTy "
func' TyConApp{} = "TyConApp "