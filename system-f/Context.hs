{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Context (
    Context (..),
    push,
    pushs,
    find,
    break3,
    fromParts,
) where

import qualified Data.List as List

newtype Context a = Context {unContext :: [a]}

instance (Show a) => Show (Context a) where
    show ctx = List.intercalate ", " $ map show (unContext ctx)

push :: a -> Context a -> Context a
push x ctx = Context (unContext ctx ++ [x])

pushs :: [a] -> Context a -> Context a
pushs xs ctx = Context (unContext ctx ++ xs)

find :: (a -> Bool) -> Context a -> Maybe a
find f ctx =
    List.find f (unContext ctx)

break3 :: (a -> Bool) -> Context a -> ([a], Maybe a, [a])
break3 f ctx =
    let xs = unContext ctx
     in case List.findIndex f xs of
            Just pos ->
                let (left, x : right) = List.splitAt pos xs
                 in (left, Just x, right)
            Nothing -> (xs, Nothing, [])

fromParts :: [a] -> a -> [a] -> Context a
fromParts left mid right = Context (left <> (mid : right))
