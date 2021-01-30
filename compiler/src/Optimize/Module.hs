{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize.Module
  ( optimize
  )
  where
import Debug.Trace (trace)


import Prelude hiding (cycle)
import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
import Data.Map ((!))
import GHC.Word (Word16)

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Utils.Type as Type
import qualified Canonicalize.Effects as Effects
import qualified Elm.ModuleName as ModuleName
import qualified Optimize.Expression as Expr
import qualified Optimize.Names as Names
import qualified Optimize.Port as Port
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Main as E
import qualified Reporting.Result as Result
import qualified Reporting.Warning as W



-- OPTIMIZE


type Result i w a =
  Result.Result i w E.Error a


type Annotations =
  Map.Map Name.Name Can.Annotation


optimize :: Annotations -> Can.Module -> Result i [W.Warning] Opt.LocalGraph
optimize annotations (Can.Module (A.At region home) _ _ decls unions aliases _ effects) =
  addDecls home annotations decls $
    addEffects home effects $
      addUnions home unions $
        addAliases home aliases $
          Opt.LocalGraph [] Map.empty Map.empty region



-- UNION


type Nodes =
  Map.Map Opt.Global Opt.Node


addUnions :: ModuleName.Canonical -> Map.Map Name.Name Can.Union -> Opt.LocalGraph -> Opt.LocalGraph
addUnions home unions (Opt.LocalGraph main nodes fields moduleName) =
  Opt.LocalGraph main (Map.foldr (addUnion home) nodes unions) fields moduleName


addUnion :: ModuleName.Canonical -> Can.Union -> Nodes -> Nodes
addUnion home (Can.Union _ ctors _ opts) nodes =
  List.foldl' (addCtorNode home opts) nodes ctors


addCtorNode :: ModuleName.Canonical -> Can.CtorOpts -> Nodes -> Can.Ctor -> Nodes
addCtorNode home opts nodes (Can.Ctor name index numArgs _) =
  let
    node =
      case opts of
        Can.Normal -> Opt.Ctor index numArgs
        Can.Unbox -> Opt.Box
        Can.Enum -> Opt.Enum index
  in
  Map.insert (Opt.Global home name) node nodes



-- ALIAS


addAliases :: ModuleName.Canonical -> Map.Map Name.Name Can.Alias -> Opt.LocalGraph -> Opt.LocalGraph
addAliases home aliases graph =
  Map.foldrWithKey (addAlias home) graph aliases


addAlias :: ModuleName.Canonical -> Name.Name -> Can.Alias -> Opt.LocalGraph -> Opt.LocalGraph
addAlias home name (Can.Alias _ tipe) graph@(Opt.LocalGraph main nodes fieldCounts moduleName) =
  case tipe of
    Can.TRecord fields Nothing ->
      let
        function =
          Opt.Function (map fst (Can.fieldsToList fields)) $ Opt.Record $
            Map.mapWithKey (\field _ -> Opt.VarLocal field) fields

        node =
          Opt.Define function Set.empty
      in
      Opt.LocalGraph
        main
        (Map.insert (Opt.Global home name) node nodes)
        (Map.foldrWithKey addRecordCtorField fieldCounts fields)
        moduleName

    _ ->
      graph


addRecordCtorField :: Name.Name -> Can.FieldType -> Map.Map Name.Name Int -> Map.Map Name.Name Int
addRecordCtorField name _ fields =
  Map.insertWith (+) name 1 fields



-- ADD EFFECTS


addEffects :: ModuleName.Canonical -> Can.Effects -> Opt.LocalGraph -> Opt.LocalGraph
addEffects home effects graph@(Opt.LocalGraph main nodes fields moduleName) =
  case effects of
    Can.NoEffects ->
      graph

    Can.Ports ports ->
      Map.foldrWithKey (addPort home) graph ports

    Can.Manager _ _ _ manager ->
      let
        fx = Opt.Global home "$fx$"
        cmd = Opt.Global home "command"
        sub = Opt.Global home "subscription"
        link = Opt.Link fx
        newNodes =
          case manager of
            Can.Cmd _ ->
              Map.insert cmd link $
              Map.insert fx (Opt.Manager Opt.Cmd) nodes

            Can.Sub _ ->
              Map.insert sub link $
              Map.insert fx (Opt.Manager Opt.Sub) nodes

            Can.Fx _ _ ->
              Map.insert cmd link $
              Map.insert sub link $
              Map.insert fx (Opt.Manager Opt.Fx) nodes
      in
      Opt.LocalGraph main newNodes fields moduleName


addPort :: ModuleName.Canonical -> Name.Name -> Can.Port -> Opt.LocalGraph -> Opt.LocalGraph
addPort home name port_ graph =
  case port_ of
    Can.Incoming _ payloadType _ ->
      let
        (deps, fields, decoder) = Names.run (Port.toDecoder payloadType)
        node = Opt.PortIncoming decoder deps
      in
      addToGraph (Opt.Global home name) node fields graph

    Can.Outgoing _ payloadType _ ->
      let
        (deps, fields, encoder) = Names.run (Port.toEncoder payloadType)
        node = Opt.PortOutgoing encoder deps
      in
      addToGraph (Opt.Global home name) node fields graph



-- HELPER


addToGraph :: Opt.Global -> Opt.Node -> Map.Map Name.Name Int -> Opt.LocalGraph -> Opt.LocalGraph
addToGraph name node fields (Opt.LocalGraph main nodes fieldCounts moduleName) =
  Opt.LocalGraph
    main
    (Map.insert name node nodes)
    (Map.unionWith (+) fields fieldCounts)
    moduleName



-- ADD DECLS


addDecls :: ModuleName.Canonical -> Annotations -> Can.Decls -> Opt.LocalGraph -> Result i [W.Warning] Opt.LocalGraph
addDecls home annotations decls graph =
  case decls of
    Can.Declare def subDecls ->
      addDecls home annotations subDecls =<< addDef home annotations def graph

    Can.DeclareRec d ds subDecls ->
      let defs = d:ds in
      case findMain defs of
        Nothing ->
          addDecls home annotations subDecls (addRecDefs home defs graph)

        Just region ->
          Result.throw $ E.BadCycle region (defToName d) (map defToName ds)

    Can.SaveTheEnvironment ->
      Result.ok graph


findMain :: [Can.Def] -> Maybe A.Region
findMain defs =
  case defs of
    [] ->
      Nothing

    def:rest ->
      case def of
        Can.Def (A.At region name) _ _ _ ->
          if name == Name._main then Just region else findMain rest

        Can.TypedDef (A.At region name) _ _ _ _ _ ->
          if name == Name._main then Just region else findMain rest


defToName :: Can.Def -> Name.Name
defToName def =
  case def of
    Can.Def (A.At _ name) _ _ _          -> name
    Can.TypedDef (A.At _ name) _ _ _ _ _ -> name



-- ADD DEFS


addDef :: ModuleName.Canonical -> Annotations -> Can.Def -> Opt.LocalGraph -> Result i [W.Warning] Opt.LocalGraph
addDef home annotations def graph =
  case def of
    Can.Def (A.At region name) args body bodyRegion ->
      do  let (Can.Forall _ tipe) = annotations ! name
          Result.warn $ W.MissingTypeAnnotation region name tipe
          addDefHelp annotations home name args body bodyRegion graph

    Can.TypedDef (A.At region name) _ typedArgs body _ bodyRegion ->
      addDefHelp annotations home name (map fst typedArgs) body bodyRegion graph


addDefHelp :: Annotations -> ModuleName.Canonical -> Name.Name -> [Can.Pattern] -> Can.Expr -> A.Region -> Opt.LocalGraph -> Result i w Opt.LocalGraph
addDefHelp annotations home name args body bodyRegion graph =
  let
    (Can.Forall _ tipe) = annotations ! name
  in
    case tipe of
      Can.TType typeRegion hm nm [_] | hm == ModuleName.embed && nm == Name.task ->
        let
          generator =
            Opt.Generator (Opt.Global home name) typeRegion bodyRegion
        in
        Result.ok (addGenerator home name body generator graph)

      _ ->
        Result.ok (addDefNode home name args body Set.empty graph)


addGenerator :: ModuleName.Canonical -> Name.Name -> Can.Expr -> Opt.Generator -> Opt.LocalGraph -> Opt.LocalGraph
addGenerator home name body generator (Opt.LocalGraph generators nodes fields moduleName) =
  addDefNode home name [] body (Set.fromList [Opt.Global home name]) (Opt.LocalGraph (generator:generators) nodes fields moduleName)


addDefNode :: ModuleName.Canonical -> Name.Name -> [Can.Pattern] -> Can.Expr -> Set.Set Opt.Global -> Opt.LocalGraph -> Opt.LocalGraph
addDefNode home name args body mainDeps graph =
  let
    (deps, fields, def) =
      Names.run $
        case args of
          [] ->
            Expr.optimize Set.empty body

          _ ->
            do  (argNames, destructors) <- Expr.destructArgs args
                obody <- Expr.optimize Set.empty body
                pure $ Opt.Function argNames $
                  foldr Opt.Destruct obody destructors
  in
  addToGraph (Opt.Global home name) (Opt.Define def (Set.union deps mainDeps)) fields graph



-- ADD RECURSIVE DEFS


data State =
  State
    { _values :: [(Name.Name, Opt.Expr)]
    , _functions :: [Opt.Def]
    }


addRecDefs :: ModuleName.Canonical -> [Can.Def] -> Opt.LocalGraph -> Opt.LocalGraph
addRecDefs home defs (Opt.LocalGraph main nodes fieldCounts moduleName) =
  let
    names = reverse (map toName defs)
    cycleName = Opt.Global home (Name.fromManyNames names)
    cycle = foldr addValueName Set.empty defs
    links = foldr (addLink home (Opt.Link cycleName)) Map.empty defs

    (deps, fields, State values funcs) =
      Names.run $
        foldM (addRecDef cycle) (State [] []) defs
  in
  Opt.LocalGraph
    main
    (Map.insert cycleName (Opt.Cycle names values funcs deps) (Map.union links nodes))
    (Map.unionWith (+) fields fieldCounts)
    moduleName


toName :: Can.Def -> Name.Name
toName def =
  case def of
    Can.Def      (A.At _ name) _ _ _     -> name
    Can.TypedDef (A.At _ name) _ _ _ _ _ -> name


addValueName :: Can.Def -> Set.Set Name.Name -> Set.Set Name.Name
addValueName def names =
  case def of
    Can.Def      (A.At _ name)   args _   _ -> if null args then Set.insert name names else names
    Can.TypedDef (A.At _ name) _ args _ _ _ -> if null args then Set.insert name names else names


addLink :: ModuleName.Canonical -> Opt.Node -> Can.Def -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global Opt.Node
addLink home link def links =
  case def of
    Can.Def (A.At _ name) _ _ _->
      Map.insert (Opt.Global home name) link links

    Can.TypedDef (A.At _ name) _ _ _ _ _ ->
      Map.insert (Opt.Global home name) link links



-- ADD RECURSIVE DEFS


addRecDef :: Set.Set Name.Name -> State -> Can.Def -> Names.Tracker State
addRecDef cycle state def =
  case def of
    Can.Def (A.At _ name) args body _ ->
      addRecDefHelp cycle state name args body

    Can.TypedDef (A.At _ name) _ args body _ _ ->
      addRecDefHelp cycle state name (map fst args) body


addRecDefHelp :: Set.Set Name.Name -> State -> Name.Name -> [Can.Pattern] -> Can.Expr -> Names.Tracker State
addRecDefHelp cycle (State values funcs) name args body =
  case args of
    [] ->
      do  obody <- Expr.optimize cycle body
          pure $ State ((name, obody) : values) funcs

    _:_ ->
      do  odef <- Expr.optimizePotentialTailCall cycle name args body
          pure $ State values (odef : funcs)
