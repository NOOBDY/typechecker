module Errors where

import Ast (Name, Type)

data TypeError
    = UnboundVariable {_name :: Name, _expr :: Maybe String}
    | ApplicationTypeError {_actual :: Type, _expr :: Maybe String}
    | TypeApplicationError {_actual :: Type, _expr :: Maybe String}
    | OccursCheck {_var :: String, _ty :: Type, _expr :: Maybe String}
    | SubtypingError {_left :: Type, _right :: Type, _expr :: Maybe String}
    | InstantiationError {_var :: String, _ty :: Type, _expr :: Maybe String}

instance Show TypeError where
    show err = case err of
        (UnboundVariable name expr) ->
            "Variable " <> show name <> " not found in context" <> aux expr
        (ApplicationTypeError actual expr) ->
            "Expected function type in application, got " <> show actual <> aux expr
        (TypeApplicationError actual expr) ->
            "Expected forall type in type application, got " <> show actual <> aux expr
        (OccursCheck var ty expr) ->
            "Occurs check failed: variable " <> show var <> " occurs in type " <> show ty <> aux expr
        (SubtypingError left right expr) ->
            "No matching rule for subtyping " <> show left <> " <: " <> show right <> aux expr
        (InstantiationError var ty expr) ->
            "No matching rule for instantiation " <> show var <> " :=< " <> show ty <> aux expr
      where
        aux expr = case expr of
            Just e -> "\n    When typing expression: " <> show e
            Nothing -> ""
