import Tools

--
-- PATTERN MATCHING
--

lucky :: Integral a => a -> String
lucky 7 = "Hey, it's lucky!"
lucky _ = "Ehh...try again."

-- Patterns match top to bottom
sayMe :: Integral a => a -> String
sayMe 1 = "one"
sayMe 2 = "two"
sayMe 3 = "three"
sayMe 4 = "four"
sayMe 5 = "five"
sayMe _ = "some other number"

-- Recursive factorial
factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
