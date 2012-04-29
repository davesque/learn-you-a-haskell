import Tools

{-
 - TYPES
 -}

removeNonUpperCase :: String -> String
removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

-- Int type is 32-bit
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Integer type has arbitrary size
factorial :: Integer -> Integer
factorial n = product [1..n]

{-
 - Typeclasses indicate if some type is compatible with a certain behavior.
 -
 - Prelude> :t (==)
 - (==) :: Eq a => a -> a -> Bool
 -
 - The "Eq a" indicates a type constraint on the `a` type variable.  Types
 - represented in the type signature of (==) by `a` must be members of the Eq
 - typeclass, which means they can be tested for equality.
 -}

testShow = show True === "True"
testRead = read "True" === True
testReadNeedsTypeSignature = read "0" :: Int

-- Convert an integral value into a more general Num value
testFromIntegral = (fromIntegral (length [1..5])) + 3.2
