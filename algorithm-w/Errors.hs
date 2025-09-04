module Errors where
import Ast (Type, Name)

data InferenceError
    = OccursCheck Name Type -- var type
    | TupleLengthMismatch Int Int -- leftLength rightLength
    | UnificationFailure Type Type -- expected actual
    | UnboundVariable Name -- name
    deriving (Show)

type Result = Either InferenceError
