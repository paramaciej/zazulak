module Main where

import           Jastarnia
import           RuntimeFahrstrasse
import           RuntimeSchema
import           SignalBoxProgram


main :: IO ()
main = signalBoxProgram (getRuntimeSchema jastarniaSchema)
    [ getRuntimeFahrstrasse jastarniaFahr1
    , getRuntimeFahrstrasse jastarniaFahr2
    , getRuntimeFahrstrasse jastarniaFahr3
    , getRuntimeFahrstrasse jastarniaFahr4
    , getRuntimeFahrstrasse jastarniaFahr5
    , getRuntimeFahrstrasse jastarniaFahr6
    , getRuntimeFahrstrasse jastarniaFahr7
    , getRuntimeFahrstrasse jastarniaFahr8
    ]
