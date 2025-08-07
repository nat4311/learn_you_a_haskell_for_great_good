{- next up: https://learnyouahaskell.github.io/higher-order-functions.html#higher-orderism
 -}

lucky :: (Integral x, Integral y) => x -> y -> String
lucky 7 7 = "Lucky"
lucky x y = "Unlucky"

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x-1)

charName :: Char -> String
charName 'a' = "abi"

sumThrees (a:b:c:xs) = (a+b+c) : sumThrees xs
sumThrees _ = []

tell :: (Show a) => [a] -> String
tell [] = "empty"
tell (x:[]) = "1 element: " ++ show x
tell _ = "2 or more elements"

l :: [a] -> Integer
l [] = 0
l (a:xs) = 1 + l xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "empty string"
capital s@(x:xs) = "1st letter of " ++ s ++ " is " ++ [x]

waterState :: (RealFloat t) => t -> String
waterState t
	| t <= 0 = "ice"
	| t < 100 = "water"
	| otherwise = "steam"

-- max' :: (Ord a) => [a] -> a
-- max' (x:[]) = x
-- max' (x:xs) = 
-- 	let b = max' xs
-- 	in if x > b then x else b

max' :: (Ord a) => [a] -> a
max' (x:[]) = x
max' (x:xs)
	| x > max' xs = x
	| otherwise = max' xs

densityTell :: (RealFloat a) => a -> a -> String  
densityTell mass volume  
    | density < air = "Wow! You're going for a ride in the sky!"  
    | density <= water = "Have fun swimming, but watch out for sharks!"  
    | otherwise   = "If it's sink or swim, you're going to sink."  
    where density = mass / volume  
          air = 1.2  
          water = 1000.0  

-- why does this throw an error
calcDistances :: (RealFloat a) => [(a,a,a)] -> [a]
calcDistances xs = [distance x y z| (x,y,z) <- xs]
	where distance x y z = sqrt (x^2 + y^2 + z^2)

whoIsIt :: String -> String
whoIsIt name = "it's " ++ case name of "Nat"    -> "me"
                                       "Abi"    -> "sister"
                                       "Andrea" -> "mom"
                                       "Tigger" -> "boy dog"
                                       "Winnie" -> "big girl dog"
                                       "Daisy"  -> "tiny girl dog"
                                       _        -> "somebody"

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."  
                                               xs -> "a longer list."  

xOr4 = max 4

zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ [] _ = []
zipwith' _ _ [] = []
zipwith' f (x:xs) (y:ys) = f x y:zipwith' f xs ys

oddSquaresLessThan10000 = sum (takeWhile (<10000) (filter odd [x^2 | x <- [1..]]))
oddSquaresLessThan10000b = sum (takeWhile (<10000) (filter odd (map (^2)[1..])))

collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
	| even n = n:collatzChain (div n 2)
	| odd n = n:collatzChain (n*3 + 1)

collatz_chains = map collatzChain [1..100]
collatz_lengths = map length collatz_chains
collatzAnswer = sum [if x>15 then 1 else 0 | x <- collatz_lengths]
collatzAnswer2 :: Int  
collatzAnswer2 = length (filter isLong (map collatzChain [1..100]))  
    where isLong xs = length xs > 15 


main = do
	putStrLn ""

	-- print collatzAnswer
	-- print collatzAnswer2

	-- print oddSquaresLessThan10000 
	-- print oddSquaresLessThan10000b

	-- print (zipwith' (+) [1..10] [11..20])

	-- print (whoIsIt "Nat")
	-- print (describeList [])
	-- print (xOr4 7)
	-- print (xOr4 3)
	-- print (max' [1,3,19,82828,2,2,442,25])
	-- print (densityTell 10000 1)
	-- print (calcDistances [(1,2,3), (3,4,0)])

	-- print (waterState (0))
	-- print (waterState 10)
	-- print (waterState 100)

	-- print (factorial 35)
	-- print (sumThrees [10,20..100])
	--
	-- putStrLn (tell ([] :: [String]))
	-- putStrLn (tell [2])
	-- putStrLn (tell [1,2,3])
