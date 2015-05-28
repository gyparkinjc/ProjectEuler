import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Data.Array
import Data.List.Split
import Data.Time.Clock
import System.IO
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Exception
    
fibs = L.map fst $ iterate (\(a, b) -> (b, a+b)) (1, 1)
primes = 2 : filter is_prime [3,5..] :: [Integer]
is_prime n
    | n == 1    = False
    | n == 2    = True
    | even n    = False
    | otherwise = all (\x -> n `mod` x /= 0) [3,5..(ceiling . sqrt . fromIntegral) n]

triangleNums = scanl1 (+) [1..]
primeFactors num = 
    let factors n all@(x:xs)
            | n == 1         = []
            | n `mod` x == 0 = x : (factors (n `div` x) all)
            | otherwise      = factors n xs
    in factors num primes
divisors num =
    let factors = primeFactors num
        lists = [product list | list <- (L.subsequences factors)]
    in L.nub lists

is_palindrome num = show num == reverse (show num)

permutation [] = [[]]
permutation xs = [x:ys | x <- xs, ys <- permutation (L.delete x xs)]

convertInt xs = (read . concat . map show ) xs :: Integer
convertDigit n = (map digitToInt . show) n

shiftR n xs = drop n xs ++ take n xs
rotationR xs = [shiftR n xs | n <- [0..(length xs)-1]]

        
solution_1 = return (sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0])
solution_2 = return (sum . takeWhile (< 4000000) . filter even $ fibs)
solution_3 = do
    let factor value p_seq
            | value == 1         = p
            | value `mod` p == 0 = factor (value `div` p) p_seq
            | otherwise          = factor value (tail p_seq)
            where p = head p_seq
    return $ toInteger (factor 600851475143 primes)
solution_4 = return (maximum $ filter is_palindrome [p * q | p <- [100..999], q <- [p..999]])
solution_5 = return (foldl lcm 1 [2..20])
solution_6 = return (abs $ (sum $ L.map (^2) [1..100]) - ((^2) $ sum [1..100]))
solution_7 = return $ toInteger $ primes !! 10000
solution_8 = do    
    let slice [] = [[1]]
        slice all@(x:xs) = (L.map (read . (: [])) (take 5 all)) : slice xs
    digits <- readFile "../data/number_8.txt"
    return (maximum $ L.map (foldl (*) 1) $ (slice . concat . lines) digits)
solution_9 = return $ 
    head [p | a <- [2..999], c <- [a..999], 
            let p = a*(1000-(a+c))*c
                t1 = 1000 - a
                t2 = a * a
                conditions = t2 `mod` t1 == 0 && 2*c == t1 + (t2 `div` t1),
            conditions]
solution_10 = return $ toInteger $ (sum $ takeWhile (< 2000000) primes)
solution_11 = do
    contents <- readFile "../data/20x20.txt"
    let oneLine = replicate 20 1
        edge = replicate 3 1
        grid = L.map ((L.map read) . words) $ lines contents :: [[Integer]]
        makeLine line = edge ++ line ++ edge
        _G = L.map makeLine (replicate 3 oneLine ++ grid ++ replicate 3 oneLine)
        maxAll' x y = 
            maximum [product [_G !! (x+i) !! (y-i) | i <- [0..3]]
                    ,product [_G !! (x+i) !! y     | i <- [0..3]]
                    ,product [_G !! (x+i) !! (y+i) | i <- [0..3]]
                    ,product [_G !! x     !! (y+i) | i <- [0..3]]
                    ]
    return $ maximum [maxAll' x y | x <- [3..22], y <- [3..22]]
solution_12 = do
    let reduce [] a = [a]
        reduce all@((a,b):ps) (p,e) = 
            if p == a 
                then (p,(b+1)) : ps 
                else (p,e) : all
        factors n = foldl reduce [] $ map (\x -> (x, 1)) $ primeFactors n
        numOfDivisor n =
             foldl (*) 1 $ map (\(p,e) -> e + 1) $ factors n
    return $ (toInteger . head) [x | x <- triangleNums, numOfDivisor x > 500]
solution_13 = do
    contents <- readFile "../data/numbers.txt"
    let digits = map read $ lines contents :: [Integer]
    return $ read $ take 10 $ (show . sum) digits 
solution_14 = do
    let n = 999999 
        collatzSeq = listArray (1, n) $ 1:[collatz x x | x <- [2..n]]
        collatz base x =
            if x < base then collatzSeq ! x else 1 + collatz base x'
            where
            x' = if even x then x `div` 2 else 3 * x + 1
        max' = (\acc x -> if snd acc < snd x then x else acc)
    return $ (fst . foldl1 max' . assocs) collatzSeq
solution_15 = return $ iterate (scanl1 (+)) (repeat 1) !! 20 !! 20
solution_16 = return $ (toInteger . sum) $ map digitToInt $ show (2^1000)
solution_17 = do
    let one = ["one", "two", "three", "four", "five", "six", "seven", "eight",
               "nine","ten", "eleven", "twelve", "thirteen", "fourteen", 
               "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
        ten = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy",
               "eighty", "ninety"]
        convert all@(x:xs)
            | len == 4 = "onethousand"
            | len == 3 = (one !! (digit-1)) ++ "hundred"
                          ++ if num `mod` 100 == 0 
                                then "" 
                                else "and" ++ convert xs
            | len == 2 =
                let func d | d == '0'  = "" ++ convert xs
                           | d == '1'  = one !! (num-1)
                           | otherwise = ten !! (digit-2) ++ convert xs
                in func x
            | otherwise = if x == '0' then "" else one !! (digit-1)
            where num = read all :: Int
                  digit = read [x] :: Int
                  len = length $ map digitToInt all
    return $ (toInteger . length . concat) [convert (show num) | num <- [1..1000]]
solution_18 = do
    contents <- readFile "../data/triangle_15.txt"
    let triangle = map (map read) $ map words $ lines contents :: [[Integer]]
        left xs = map init xs
        right xs = map tail xs
        maxPath (t:ts)
            | ts == []  = head t
            | otherwise = head t + (max lmax rmax)
            where lmax = maxPath $ left ts
                  rmax = maxPath $ right ts
    return $ maxPath triangle
solution_19 = do
    let days = ["Mon", "Tue", "Wed", "Thr", "Fri", "Sat", "Sun"]
        date y m d = days !! idx
            where
              isLeapYear x
                | x `mod` 400 == 0                       = True
                | (x `mod` 4 == 0) && (x `mod` 100 /= 0) = True
                | otherwise                              = False
              numLeapYear = length $ filter isLeapYear [1900..y-1]
              dayOfMonth x
                | x == 2                           = 28
                | x `elem` [1, 3, 5, 7, 8, 10, 12] = 31
                | x `elem` [4, 6, 9, 11]           = 30                    
              y_diff = y - 1900                  
              d_diff = 
                sum [dayOfMonth x | x <- [1..(m-1)]] + y_diff * 365 + numLeapYear + rest
                where rest
                        | isLeapYear y && m > 2 = d
                        | otherwise             = d - 1
              idx = d_diff `mod` 7
    return $ (toInteger . length)
           $ filter (== "Sun") [date y m 1 | y <- [1901..2000], m <- [1..12]]
solution_20 = return $ toInteger $ sum $ map digitToInt $ show (product [2..100])
solution_21 = do
    let d a = (sum . init) $ divisors a
        isAmicable a = (a == (d b)) && (a /= b)
            where b = d a
    return $ (toInteger . sum) [x | x <- [2..9999], isAmicable x]
solution_22 = do
    contents <- readFile "../data/names.txt"
    let names = map (init . tail) $ L.sort $ splitOn "," contents
        toPosNum = (\c -> (ord c) - 64)
        toStringNum = (\(x,y) -> x * (sum $ map toPosNum y))
    return $ (toInteger . sum) $ map toStringNum $ zip [1..] names
solution_23 = do
    let isAbundant n = n < ((sum . init) $ divisors n)
        a_list = [x | x <- [12..28123], isAbundant x]
        a_sumSet = S.fromList [x + y | x <- a_list, y <- a_list]
    return $ toInteger $ (sum . S.toList) $ S.difference (S.fromList [1..28123]) a_sumSet
solution_24 = do
    let num = map intToDigit $ (last . take 1000000) $ permutation [0..9]
    return $ toInteger $ read num
solution_25 = do
    let len n = length $ show n
    return $ (toInteger . head) [x | (x, y) <- zip [1..] fibs, (length $ show y) == 1000]
solution_26 = do
    let numCycle n xs
            | remainder `elem` xs = length xs
            | otherwise           = numCycle n (remainder:xs)
            where remainder = (10*(head xs)) `mod` n
        compare' = (\acc (x, y) -> if snd acc < y then (x, y) else acc)
    return $ fst $ foldl1 compare' [(n, numCycle n [1]) | n <- [2..999]]
solution_27 = return 27
solution_28 = do
    let size = 1001
        spiral = listArray (1, (size+1) `div` 2) $ [f x | x <- [1,3..size]]
        f n | n == 1    = [1]
            | otherwise = let idx = (n+1) `div` 2 - 1
                              start = (last $ spiral ! idx) + (n-1)
                          in [start + (n-1)*x | x <- [0..3]]    
    return $ (toInteger . sum . concat . map snd) $ assocs spiral
solution_29 = return $ (toInteger . length . L.nub) [a^b | a <- [2..100], b <- [2..100]]
solution_30 = do
    let func = (\n -> ((sum . map (^5) . map digitToInt . show) n) == n)
    return $ (toInteger . sum . filter func) [n | n <- [2..354294]]
solution_31 = do
    let total = 200
        coins = [1,2,5,10,20,50,100,200] :: [Int]
        numCoin = length coins
        row t = listArray (0, numCoin-1) [number t x | x <- [0,1..(numCoin-1)]]
        table = listArray (1, total) [row x | x <- [1..total]]
        number target idx
            | coins !! idx == 1      = 1 :: Int
            | coins !! idx > target  = table ! target ! (idx-1)
            | coins !! idx == target = (table ! target ! (idx-1)) + 1
            | otherwise = (table ! target ! (idx-1)) +
                          (table ! (target-(coins !! idx)) ! idx)
    return $ toInteger $ table ! total ! (numCoin-1)
solution_32 = do
    let digits = [1,2,3,4,5,6,7,8,9]
        m1x4 = do
            a <- [[x] | x <- digits]
            x <- digits L.\\ a
            y <- digits L.\\ (a ++ [x])
            z <- digits L.\\ (a ++ [x,y])
            s <- digits L.\\ (a ++ [x,y,z])
            return [a, [x,y,z,s]]
        m2x3 = do
            a <- [x:[y] | x <- digits, y <- (digits L.\\ [x])]
            x <- digits L.\\ a
            y <- digits L.\\ (a ++ [x])
            z <- digits L.\\ (a ++ [x,y])
            return [a, [x,y,z]]
        pandigit ms =
            let a = convertInt $ head ms
                b = convertInt $ last ms
                pList = convertDigit $ a*b
            in L.sort pList == digits L.\\ (head ms ++ last ms)
        p ms = a * b
            where a = convertInt $ head ms
                  b = convertInt $ last ms
        panProducts = (L.nub . map p) $ filter pandigit (m2x3 ++ m1x4)
    return $ (toInteger . sum) $ panProducts
solution_33 = do
    let nd_list = [[n,d] | n <- [10..99], d <- [n..99]]
        common ([n,d]) = L.intersect (convertDigit n) (convertDigit d)
        cnd_list = filter ((1 ==) . length . common) nd_list
        fractions = 
            filter (\all@([n,d]) -> 
                        let n_list = convertDigit n
                            d_list = convertDigit d                            
                            c = common all
                            numer = fromIntegral n
                            denom = fromIntegral d
                            cancel_n = fromIntegral (head (n_list L.\\ c))
                            cancel_d = fromIntegral (head (d_list L.\\ c))
                        in (numer / denom) == (cancel_n / cancel_d) && (head c /= 0))
                   cnd_list
        p_numer = (product . map head) fractions
        p_denom = (product . map last) fractions
    return $ toInteger $ p_denom `div` (gcd p_numer p_denom)
solution_34 = do
    let factList = 1:1:[fact n | n <- [2..]]
        fact 0 = 1
        fact 1 = 1
        fact n = (toInteger n) * factList !! (n-1)
        factSum n = sum [ fact $ digitToInt a | a <- show n ]
    return $ (toInteger . sum) $ filter (\n -> n == factSum n) [10..99999]
solution_35 = do
    let circularP n =
            let nums = (map convertInt . rotationR . convertDigit) n
            in all is_prime nums
    return $ toInteger $ length $ filter circularP $ takeWhile (<1000000) primes
solution_36 = do
    let toBin 1 = [1]
        toBin n = (toBin (n `div` 2)) ++ [n `mod` 2]
        doublePalin = (\n -> if is_palindrome n 
                             then is_palindrome $ convertInt $ toBin n
                             else False)
    return $ toInteger $ sum $ filter doublePalin $ take 1000000 [1..]
solution_37 = do
    let a_list n =
            let xs = convertDigit n
            in map convertInt [elem | f <- [drop, take], 
                                      i <- [0..(length xs)], 
                                      let elem = f i xs, elem /= []]            
        trunc n = if n < 10 then False 
                  else all is_prime $ a_list n
    return $ toInteger $ sum $ take 11 $ filter trunc $ primes
solution_38 = return 38
    -- let 
    -- return $ toInteger $ head $ filter pandigit $ permutation [9,8..0]
solution_39 = return 39
solution_40 = return 40

solutions :: [(Int, IO Integer)]    
solutions = 
    [(  1, solution_1)  ,(  2, solution_2)  ,(  3, solution_3)  ,(  4, solution_4)
    ,(  5, solution_5)  ,(  6, solution_6)  ,(  7, solution_7)  ,(  8, solution_8)
    ,(  9, solution_9)  ,( 10, solution_10) ,( 11, solution_11) ,( 12, solution_12)
    ,( 13, solution_13) ,( 14, solution_14) ,( 15, solution_15) ,( 16, solution_16)
    ,( 17, solution_17) ,( 18, solution_18) ,( 19, solution_19) ,( 20, solution_20)
    ,( 21, solution_21) ,( 22, solution_22) ,( 23, solution_23) ,( 24, solution_24)
    ,( 25, solution_25) ,( 26, solution_26) ,( 27, solution_27) ,( 28, solution_28)
    ,( 29, solution_29) ,( 30, solution_30) ,( 31, solution_31) ,( 32, solution_32)
    ,( 33, solution_33) ,( 34, solution_34) ,( 35, solution_35) ,( 36, solution_36)
    ,( 37, solution_37) ,( 38, solution_38) ,( 39, solution_39) ,( 40, solution_40)
    ]
            
main = do
    contents <- readFile "../data/euler_answer.txt"
    let answers = zip (map (read . L.delete '.' . head . words) $ lines contents :: [Int])
                      (map (last . words) $ lines contents)
    putStr "Input Problem Number : "
    hFlush stdout
    numStr <- getLine
    let num = read numStr :: Int
        (Just answer) = lookup num answers
    if num > 0 
        then solution (num, answer)
        else solution_all answers
        
solution (num, answer) = do    
    let (Just action) = lookup num solutions        
    start <- getCurrentTime
    value <- action
    evaluate value
    end <- getCurrentTime    
    t_f <- return (answer == show value)    
    if t_f 
    then print $ "[Proj : " ++ show num ++ "] " ++ show t_f ++ " " ++ show value ++ " Time: " ++ show (diffUTCTime end start)
    else print $ "False [Proj : " ++ show num ++ "] " ++ show value ++ " " ++ show t_f ++ " " ++ show (diffUTCTime end start)
    hFlush stdout

solution_all answers = do
    let paramsPair = zipWith (\(n, s) (x, a) -> (n, a)) solutions answers
    start <- getCurrentTime
    result <- sequence $ map solution paramsPair
    end <- getCurrentTime
    print $ "[Total number : " ++ show (length solutions) ++ "] " ++ show (diffUTCTime end start)
    