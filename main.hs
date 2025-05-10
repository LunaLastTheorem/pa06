-- Assume that the parameter is a binary number and return its decimal value.
binaryToDecimal :: Int -> Int
binaryToDecimal 0 = 0
binaryToDecimal i = 2 * binaryToDecimal(div i 10) + (mod i 10) 

-- takes a list of binary numbers and returns their decimal sum
addBinary :: [Int] -> Int
addBinary a = sum' (map binaryToDecimal a)
    where
     sum' [] = 0
     sum' (x:xs) = x + sum' xs

{-- takes a list of any type and returns the last element of the list. 
    If the list is empty, display a clear error message using the error function --}
last' :: [a] -> a
last' [] = error "please don't call with empty list"
last' (x:[]) = x
last' (x:xs) = last' xs

{-- returns the list without the last element. If the list is empty, display an 
    error using the error function. Don’t use the built-in function init --}
init' :: [a] -> [a]
init' [] = error "please don't call with empty list"
init' (x:[]) = []
init' (x:xs) = x : init' xs

{-- When given a list of Char (a string) returns true if the elements are the 
    same forward as backward.--}
palindrome :: [Char] -> Bool
palindrome [] = True
palindrome (x:[]) = True;
palindrome (x:xs)
    | x /= last' xs = False
    | otherwise  = palindrome (init' xs)

{-- Takes a function and a list and returns a list that contains the items of 
    the original list except for those where the function returns True. In other 
    words, removes all items from the list where the function returns True--}
filterOut :: (a -> Bool) -> [a] -> [a]
filterOut f [] = []
filterOut f (x:xs)
    | f x = filterOut f xs
    | otherwise = x : filterOut f xs

{-- Takes an Int (n) and a function and returns a list that contains the result 
    of the function applied to the numbers 0 through n-1 --}
nTimes :: Int -> (Int -> Int) -> [Int]
nTimes n f = map f [0..n-1]

{-- Takes a function and a list and returns the number of elements in the list 
    for which the function returns True --}
count :: (a -> Bool) -> [a] -> Int
count f [] = 0
count f (x:xs)
    | f x = 1 + count f xs
    | otherwise = count f xs

{-- Takes an orderable type value and a sorted list and returns a sorted list 
    with the first argument inserted into the list at an appropriate location such 
    that the list is still sorted. --}
insert :: (Ord a) => a -> [a] -> [a]
insert p [] = p : []
insert p (x:xs)
    | p < x = p : x : xs
    | otherwise = x : insert p xs

{-- Takes a list of orderable elements, sorts the elements, and returns the 
    sorted list. This must use the standard insertion sort algorithm (without loops). 
    Hint: make use of the insert function above. I suggest thinking of insertion sort 
    in the opposite direction than it is normally taught. Think of the sorted part of 
    the list growing from the right end of the list instead of the left, and 
    inserting into the list on the right instead of the left. Use the cons 
    operator (:), not the append ++ operator. --}
insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:[]) = [x]
insertionSort (x:y:ys)
    | y < x = insert y (insertionSort (x:ys))
    | otherwise = x : insertionSort (y:ys)

{-- Takes a function and two lists (in that order). The function (f) should take 
    two arguments and return a single value. The two lists do not have to be the same 
    length. The combine function will return a new list that is the same length as the 
    shorter of the two lists, and contains the results of the function applied to 
    corresponding elements of the two lists. --}
combine :: (a -> a -> a) -> [a] -> [a] -> [a]
combine f x [] = []
combine f [] y = []
combine f (x:xs) (y:ys) = f x y : combine f xs ys
    

{-- This function finds the minimum value in a list of orderable values. Note that 
    for this problem your code must be efficient. In order to make it efficient, you 
    may need to use a where clause. If it takes a noticeable amount of time on the long 
    example below, it is not efficient enough. If the list is empty, display an error 
    message.--}
minimum' :: (Ord a) => [a] -> a
minimum' [] = error "please don't call with empty list"
minimum' (x:[]) = x
minimum' (x:xs) = min' x (minimum' xs)
    where 
     min' a b
        | a < b = a
        | otherwise = b


{-- Takes a list of Bool and returns True if all of the values of the list are True. 
    If the list is empty, return True. (Do not use the built in function and.) --}
allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs)
    | x == False = False
    | otherwise = allTrue xs

{-- Takes a list of Bool and returns True if any of the values of the list are True. If the list is empty, return False. (Do not use the built in function or). --}
anyTrue :: [Bool] -> Bool
anyTrue [] = False
anyTrue (x:xs)
    | x = True
    | otherwise = anyTrue xs

{-- Takes an integer and a single value and produces a list of that value repeated n 
    times where n is the first argument. Do not use the built in function replicate. --}
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n - 1) x

{-- Takes a list of strings and returns a list containing the lengths of each string. 
    You may use the built-in functions map and length. --}
lengths :: [[Char]] -> [Int]
lengths x = map length x


{-- Takes a list of integers and returns a list of lists. Each list in the return 
    value is a list of the divisors of x where x is the corresponding value in the 
    parameter list. --}
divisors :: [Int] -> [[Int]]
divisors [] = []
divisors n = map divisor n
    where divisor n = [x | x <- [1..n], n `mod` x == 0]

{-- Returns a list of Bool. Each element in the returned list is True if the 
    corresponding integer is prime. You can solve this without recursion using map 
    and a list comprehension. Think about a variation on the list comprehension from 
    the previous problem. --}
prime :: [Int] -> [Bool]
prime = map (\n -> length (divisor n) == 2)
    where divisor n = [x | x <- [1..n], n `mod` x == 0]