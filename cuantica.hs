--Reyes Granados Naomi Itzel
import System.Random

type QuReg = [Float]

ide :: a -> a
ide x = x

plus :: QuReg -> QuReg -> QuReg
plus  = zipWith (+)

scalar :: Float -> QuReg -> QuReg
scalar a = map (a*)

repeatN :: Int -> a -> [a]
repeatN n = take n . repeat

delta :: Int -> Int -> QuReg
delta n i = repeatN i 0 ++ [1] ++ repeatN (2^n - i - 1) 0

tensor :: QuReg -> QuReg -> QuReg
tensor q r = [a*b | a <- q, b <- r]

finalise :: QuReg -> IO Int
finalise x = do i <- randomRIO (0, (length x)-1)
       	      	return (i)

trans :: (Int -> Int) -> QuReg -> QuReg
trans f q = zipWith (*) signs q
      	    where signs =  map (\x -> (-1)^(f x)) [0..2^n-1]
	    	  n = floor (logBase (fromIntegral 2) (fromIntegral (length q)))

com:: (QuReg -> QuReg) -> (QuReg -> QuReg) -> QuReg -> QuReg
com h k q = foldr1 plus (zipWith scalar q ts)
      	    where ts = [tensor p r | p <- hq, r <- kq]
	    	  hq = map (h . (delta (n-1))) [0..(2^(n-1))-1]
		  kq = map (k. (delta 1)) [0, 1]
		  n = floor (logBase (fromIntegral 2) (fromIntegral (length q)))

hadamard :: QuReg -> QuReg
hadamard q = had (length q) q
	 where h (a:b:xs)= [(1/(sqrt 2))*(a + b),(1/(sqrt 2))*(a-b)]
	       had 2 = h
	       had n = com (had (n-1)) h

ini :: Int -> QuReg
ini n = hadamard (delta n 0)

dj :: Int -> (Int -> Int) -> QuReg
dj n f = hadamard (trans f (ini n))

grover :: Int -> (Int -> Int) -> Int -> QuReg
grover n f i = (loop i (diffusion . trans f ) . ini) n
       	       where loop i f = head . drop i . iterate f
	       	     diffusion q = map (\x -> 2 * avg - x ) q
		     	       where avg = (1/(2^n)) * sum q

monadic :: QuReg -> (Int -> QuReg) -> QuReg
monadic q f = foldr1 plus (zipWith scalar q (map f [0..2^n-1]))
	  where n = floor (logBase (fromIntegral 2) (fromIntegral (length q)))