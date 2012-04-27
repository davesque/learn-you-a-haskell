module Tools where

import Control.Exception

-- Asserts equality and returns value of x
x === y = assert (x == y) x
