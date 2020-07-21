module Interview.Environment
  ( Environment(..)
  ) where

data Environment = Development
                 | Test
                 | Production
    deriving (Show, Eq, Read)

