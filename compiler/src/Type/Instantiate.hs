{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Instantiate
  ( FreeVars
  , fromSrcType
  )
  where


import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Name as Name

import qualified AST.Canonical as Can
import Type.Type



-- FREE VARS


type FreeVars =
  Map.Map Name.Name Type



-- FROM SOURCE TYPE


fromSrcType :: Map.Map Name.Name Type -> Can.Type -> IO Type
fromSrcType freeVars sourceType =
  case sourceType of
    Can.TLambda arg result ->
      FunN
        <$> fromSrcType freeVars arg
        <*> fromSrcType freeVars result

    Can.TVar name ->
      return (freeVars ! name)

    Can.TType region home name args ->
      AppN region home name <$> traverse (fromSrcType freeVars) args

    Can.TAlias region home name args aliasedType ->
      do  targs <- traverse (traverse (fromSrcType freeVars)) args
          AliasN region home name targs <$>
            case aliasedType of
              Can.Filled realType ->
                fromSrcType freeVars realType

              Can.Holey realType ->
                fromSrcType (Map.fromList targs) realType

    Can.TTuple a b maybeC ->
      TupleN
        <$> fromSrcType freeVars a
        <*> fromSrcType freeVars b
        <*> traverse (fromSrcType freeVars) maybeC

    Can.TUnit ->
      return UnitN

    Can.TRecord fields maybeExt ->
      RecordN
        <$> traverse (fromSrcFieldType freeVars) fields
        <*>
          case maybeExt of
            Nothing ->
              return EmptyRecordN

            Just ext ->
              return (freeVars ! ext)


fromSrcFieldType :: Map.Map Name.Name Type -> Can.FieldType -> IO Type
fromSrcFieldType freeVars (Can.FieldType _ tipe) =
  fromSrcType freeVars tipe
