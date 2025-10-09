module InferenceTree where

data InferenceTree = InferenceTree
    { _rule :: String
    , _input :: String
    , _output :: String
    , _children :: [InferenceTree]
    }

instance Show InferenceTree where
    show tree = aux tree 0
      where
        aux (InferenceTree rule input output children) indent =
            let prefix = concat $ replicate indent "  "
                line = prefix <> rule <> ": " <> input <> " ⇒  " <> output <> "\n"
             in line <> concatMap (\t -> aux t (indent + 1)) children
