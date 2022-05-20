{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
-- | Support for generating PIR with global definitions with dependencies between them.
module PlutusIR.Compiler.Definitions (DefT
                                              , MonadDefs (..)
                                              , TermDefWithStrictness
                                              , runDefT
                                              , defineTerm
                                              , modifyTermDef
                                              , defineType
                                              , modifyTypeDef
                                              , defineDatatype
                                              , modifyDatatypeDef
                                              , modifyDeps
                                              , recordAlias
                                              , lookupTerm
                                              , lookupOrDefineTerm
                                              , lookupType
                                              , lookupOrDefineType
                                              , lookupConstructors
                                              , lookupDestructor) where

import PlutusIR as PIR
import PlutusIR.MkPir hiding (error)

import PlutusCore.MkPlc qualified as PLC
import PlutusCore.Quote

import Control.Lens
import Control.Monad.Except
import Control.Monad.Morph qualified as MM
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AM
import Algebra.Graph.NonEmpty.AdjacencyMap qualified as NAM
import Algebra.Graph.ToGraph qualified as Graph

import Data.Bifunctor (first, second)
import Data.Foldable
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set

{- Note [Annotations on Bindings]

When constructing Bindings (including TermBind, TypeBind and DatatypeBind) from definitions
in `runDefT`, we use the annotation on the VarDecl and TyVarDecl as the annotation of the
Binding itself. The reason is that the simplifier looks at the annotation of a Binding as
one of the factors to determine whether to inline the Binding. When we define a term that
should be inlined, we put the corresponding annotation in the VarDecl or TyVarDecl (note
that there's no need to annotate the term itself), and that annotation needs to be copied
to the Binding when creating the Binding.
-}

-- | A map from keys to pairs of bindings and their dependencies (as a list of keys).
type DefMap key def = Map.Map key (def, Set.Set key)

mapDefs :: (a -> b) -> DefMap key a -> DefMap key b
mapDefs f = Map.map (first f)

type TermDefWithStrictness uni fun ann =
    PLC.Def (VarDecl TyName Name uni fun ann) (Term TyName Name uni fun ann, Strictness)

data DefState key uni fun ann = DefState {
    _termDefs     :: DefMap key (TermDefWithStrictness uni fun ann),
    _typeDefs     :: DefMap key (TypeDef TyName uni ann),
    _datatypeDefs :: DefMap key (DatatypeDef TyName Name uni fun ann),
    _aliases      :: Set.Set key
    }
makeLenses ''DefState

newtype DefT key uni fun ann m a = DefT { unDefT :: StateT (DefState key uni fun ann) m a }
    deriving newtype (Functor, Applicative, Monad, MonadTrans, MM.MFunctor, MonadError e, MonadReader r, MonadQuote, MonadWriter w)

-- Need to write this by hand, deriving wants to derive the one for DefState
instance MonadState s m => MonadState s (DefT key uni fun ann m) where
    get = lift get
    put = lift . put
    state = lift . state

runDefT :: (Monad m, Ord key) => ann -> DefT key uni fun ann m (Term TyName Name uni fun ann) -> m (Term TyName Name uni fun ann)
runDefT x act = do
    (term, s) <- runStateT (unDefT act) (DefState mempty mempty mempty mempty)
    pure $ wrapWithDefs x (bindingDefs s) term
        where
            bindingDefs defs =
                let
                    -- See Note [Annotations on Bindings]
                    terms = mapDefs (\d -> TermBind (_varDeclAnn (defVar d)) (snd $ PLC.defVal d) (PLC.defVar d) (fst $ PLC.defVal d)) (_termDefs defs)
                    types = mapDefs (\d -> TypeBind (_tyVarDeclAnn (defVar d)) (PLC.defVar d) (PLC.defVal d)) (_typeDefs defs)
                    datatypes = mapDefs (\d -> DatatypeBind (_tyVarDeclAnn (defVar d)) (PLC.defVal d)) (_datatypeDefs defs)
                in terms `Map.union` types `Map.union` datatypes

-- | Given the definitions in the program, create a topologically ordered list of the
-- SCCs using the dependency information
defSccs :: Ord key => DefMap key def -> [ NAM.AdjacencyMap key ]
defSccs tds =
    let
        perKeyDeps = fmap (\(key, (_, deps)) -> (key, deps)) (Map.assocs tds)
        keySccs = AM.scc (AM.fromAdjacencySets perKeyDeps)
    -- the graph made by 'scc' is guaranteed to be acyclic
    in case AM.topSort keySccs of
        Right sorted -> sorted
        -- TODO: report cycle
        Left _       -> error "No topological sort of SCC graph"

wrapWithDefs
    :: Ord key
    => ann
    -> DefMap key (Binding tyname name uni fun ann)
    -> Term tyname name uni fun ann
    -> Term tyname name uni fun ann
wrapWithDefs x tds body =
    let toValue k = fst <$> Map.lookup k tds
        wrapDefScc acc scc =
            let bs = catMaybes $ toValue <$> Graph.vertexList scc
            in mkLet x (if Graph.isAcyclic scc then NonRec else Rec) bs acc
    -- process from the inside out
    in foldl' wrapDefScc body (defSccs tds)

class (Monad m, Ord key) => MonadDefs key uni fun ann m | m -> key uni fun ann where
    liftDef :: DefT key uni fun ann Identity a -> m a
    default liftDef :: (MonadDefs key uni fun ann n, MonadTrans t, t n ~ m) => DefT key uni fun ann Identity a -> m a
    liftDef = lift . liftDef

instance (Ord key, Monad m) => MonadDefs key uni fun ann (DefT key uni fun ann m) where
    liftDef = MM.hoist (pure . runIdentity)

instance MonadDefs key uni fun ann m => MonadDefs key uni fun ann (StateT s m)
instance MonadDefs key uni fun ann m => MonadDefs key uni fun ann (ExceptT e m)
instance MonadDefs key uni fun ann m => MonadDefs key uni fun ann (ReaderT r m)

defineTerm :: MonadDefs key uni fun ann m => key -> TermDefWithStrictness uni fun ann -> Set.Set key -> m ()
defineTerm name def deps = liftDef $ DefT $ modify $ over termDefs $ Map.insert name (def, deps)

modifyTermDef :: MonadDefs key uni fun ann m => key -> (TermDefWithStrictness uni fun ann -> TermDefWithStrictness uni fun ann)-> m ()
modifyTermDef name f = liftDef $ DefT $ modify $ over termDefs $ Map.adjust (first f) name

defineType :: MonadDefs key uni fun ann m => key -> TypeDef TyName uni ann -> Set.Set key -> m ()
defineType name def deps = liftDef $ DefT $ modify $ over typeDefs $ Map.insert name (def, deps)

modifyTypeDef :: MonadDefs key uni fun ann m => key -> (TypeDef TyName uni ann -> TypeDef TyName uni ann)-> m ()
modifyTypeDef name f = liftDef $ DefT $ modify $ over typeDefs $ Map.adjust (first f) name

defineDatatype
    :: forall key uni fun ann m . MonadDefs key uni fun ann m
    => key -> DatatypeDef TyName Name uni fun ann -> Set.Set key -> m ()
defineDatatype name def deps = liftDef $ DefT $ modify $ over datatypeDefs $ Map.insert name (def, deps)

modifyDatatypeDef :: MonadDefs key uni fun ann m => key -> (DatatypeDef TyName Name uni fun ann -> DatatypeDef TyName Name uni fun ann)-> m ()
modifyDatatypeDef name f = liftDef $ DefT $ modify $ over datatypeDefs $ Map.adjust (first f) name

-- | Modifies the dependency set of a key.
modifyDeps :: MonadDefs key uni fun ann m => key -> (Set.Set key -> Set.Set key)-> m ()
modifyDeps name f = liftDef $ DefT $ do
    -- This is a little crude: we expect most keys will appear in only one map, so we just modify the
    -- dependencies in all of them! That lets us just have one function.
    modify $ over termDefs $ Map.adjust (second f) name
    modify $ over typeDefs $ Map.adjust (second f) name
    modify $ over datatypeDefs $ Map.adjust (second f) name

recordAlias :: forall key uni fun ann m . MonadDefs key uni fun ann m => key -> m ()
recordAlias name = liftDef @key @uni @fun @ann $ DefT $ modify $ over aliases (Set.insert name)

lookupTerm :: (MonadDefs key uni fun ann m) => ann -> key -> m (Maybe (Term TyName Name uni fun ann))
lookupTerm x name = do
    DefState{_termDefs=ds,_aliases=as} <- liftDef $ DefT get
    pure $ case Map.lookup name ds of
        Just (def, _) | not (Set.member name as) -> Just $ mkVar x $ PLC.defVar def
        _                                        -> Nothing

lookupOrDefineTerm :: (MonadDefs key uni fun ann m) => ann -> key -> m (TermDefWithStrictness uni fun ann, Set.Set key) -> m (Term TyName Name uni fun ann)
lookupOrDefineTerm x name mdef = do
    mTerm <- lookupTerm x name
    case mTerm of
        Just t -> pure t
        Nothing -> do
            (def, deps) <- mdef
            defineTerm name def deps
            pure $ mkVar x $ PLC.defVar def

lookupType :: (MonadDefs key uni fun ann m) => ann -> key -> m (Maybe (Type TyName uni ann))
lookupType x name = do
    DefState{_typeDefs=tys, _datatypeDefs=dtys, _aliases=as} <- liftDef $ DefT get
    pure $ case Map.lookup name tys of
        Just (def, _) -> Just $ if Set.member name as then PLC.defVal def else mkTyVar x $ PLC.defVar def
        Nothing -> case Map.lookup name dtys of
            Just (def, _) -> Just $ mkTyVar x $ PLC.defVar def
            Nothing       -> Nothing

lookupOrDefineType :: (MonadDefs key uni fun ann m) => ann -> key -> m (TypeDef TyName uni ann, Set.Set key) -> m (Type TyName uni ann)
lookupOrDefineType x name mdef = do
    mTy <- lookupType x name
    case mTy of
        Just ty -> pure ty
        Nothing -> do
            (def, deps) <- mdef
            defineType name def deps
            pure $ mkTyVar x $ PLC.defVar def

lookupConstructors :: (MonadDefs key uni fun ann m) => ann -> key -> m (Maybe [Term TyName Name uni fun ann])
lookupConstructors x name = do
    ds <- liftDef $ DefT $ use datatypeDefs
    pure $ case Map.lookup name ds of
        Just (PLC.Def{PLC.defVal=(Datatype _ _ _ _ constrs)}, _) -> Just $ fmap (mkVar x) constrs
        Nothing                                                  -> Nothing

lookupDestructor
    :: forall key uni fun ann m . (MonadDefs key uni fun ann m)
    => ann -> key -> m (Maybe (Term TyName Name uni fun ann))
lookupDestructor x name = do
    ds <- liftDef @key @uni @fun @ann $ DefT $ use datatypeDefs
    pure $ case Map.lookup name ds of
        Just (PLC.Def{PLC.defVal=(Datatype _ _ _ destr _)}, _) -> Just $ Var x destr
        Nothing                                                -> Nothing
