{-# LANGUAGE GADTs #-}
module Printer where

import           Schema

class SchemaShow a where
    sShow :: a -> String

instance SchemaShow (Schema s t l) where
    sShow _ = "elo"

instance SchemaShow (Track n l1 l2) where
    sShow (Track len) = replicate len '-'
